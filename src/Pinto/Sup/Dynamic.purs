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
import Pinto.Types (RegistryInstance, RegistryName, RegistryReference, StartLinkResult, registryInstance)

newtype DynamicType :: Type -> Type -> Type
newtype DynamicType childStartArg childProcess
  = DynamicType Void

newtype DynamicPid :: Type -> Type -> Type
newtype DynamicPid childStartArg childProcess
  = DynamicPid Pid

derive newtype instance supervisorPidHasPid :: HasPid (DynamicPid childStartArg childProcess)

type DynamicRef childStartArg childProcess
  = RegistryReference (DynamicPid childStartArg childProcess) (DynamicType childStartArg childProcess)

type DynamicInstance childStartArg childProcess
  = RegistryInstance (DynamicPid childStartArg childProcess) (DynamicType childStartArg childProcess)

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
  DynamicRef childStartArg childProcess ->
  childStartArg ->
  Effect (StartChildResult childProcess)
startChild = startChildFFI <<< registryInstance

foreign import startChildFFI ::
  forall childStartArg childProcess.
  DynamicInstance childStartArg childProcess ->
  childStartArg ->
  Effect (StartChildResult childProcess)
