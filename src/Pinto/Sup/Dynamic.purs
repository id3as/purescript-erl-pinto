module Pinto.Sup.Dynamic
  ( DynamicSpec
  , DynamicRef(..)
  , DynamicPid
  , DynamicType
  , startLink
  , startChild
  ) where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Process.Raw (Pid, class HasPid)
import Pinto.Sup (ChildShutdownTimeoutStrategy, ChildType, RestartStrategy, Seconds, StartChildResult)
import Pinto.Types (RegistryName, StartLinkResult)

newtype DynamicType :: Type -> Type -> Type
newtype DynamicType childStartArg childProcess
  = DynamicType Void

newtype DynamicPid :: Type -> Type -> Type
newtype DynamicPid childStartArg childProcess
  = DynamicPid Pid

derive newtype instance supervisorPidHasPid :: HasPid (DynamicPid childStartArg childProcess)

data DynamicRef childStartArg childProcess
  = ByName (RegistryName (DynamicType childStartArg childProcess))
  | ByPid (DynamicPid childStartArg childProcess)

type DynamicSpec childStartArg childProcess
  = { intensity :: Int
    , period :: Seconds
    , start :: childStartArg -> Effect (StartLinkResult childProcess)
    , restartStrategy :: RestartStrategy
    , shutdownStrategy :: ChildShutdownTimeoutStrategy
    , childType :: ChildType
    }

startLink ::
  forall childStartArg childProcess.
  HasPid childProcess =>
  Maybe (RegistryName (DynamicType childStartArg childProcess)) ->
  Effect (DynamicSpec childStartArg childProcess) ->
  Effect (StartLinkResult (DynamicPid childStartArg childProcess))
startLink = startLinkFFI

foreign import startLinkFFI ::
  forall childStartArg childProcess.
  Maybe (RegistryName (DynamicType childStartArg childProcess)) ->
  Effect (DynamicSpec childStartArg childProcess) ->
  Effect (StartLinkResult (DynamicPid childStartArg childProcess))

startChild ::
  forall childStartArg childProcess.
  HasPid childProcess =>
  childStartArg ->
  DynamicRef childStartArg childProcess ->
  Effect (StartChildResult childProcess)
startChild = startChildFFI

foreign import startChildFFI ::
  forall childStartArg childProcess.
  childStartArg ->
  DynamicRef childStartArg childProcess ->
  Effect (StartChildResult childProcess)
