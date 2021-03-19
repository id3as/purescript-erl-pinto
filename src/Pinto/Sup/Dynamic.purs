module Pinto.Sup.Dynamic
  ( DynamicSpec
  , DynamicRef(..)
  , DynamicPid
  , DynamicType
  , startLink
  , startChild
  ) where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Pinto.Sup (Millisecond, Seconds, RestartStrategy, Strategy, ChildType, ChildShutdownTimeoutStrategy, ChildStarted, ChildNotStartedReason, StartChildResult)
import Pinto.Types (RegistryName, StartLinkResult, class HasRawPid)

newtype DynamicType childStartArg childProcess = DynamicType Void
newtype DynamicPid childStartArg childProcess = DynamicPid Pid
derive newtype instance supervisorPidHasRawPid :: HasRawPid (DynamicPid childStartArg childProcess)

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
  forall childStartArg childProcess. HasRawPid childProcess =>
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
  forall childStartArg childProcess. HasRawPid childProcess =>
  childStartArg ->
  DynamicRef childStartArg childProcess ->
  Effect (StartChildResult childProcess)
startChild = startChildFFI

foreign import startChildFFI ::
  forall childStartArg childProcess.
  childStartArg ->
  DynamicRef childStartArg childProcess ->
  Effect (StartChildResult childProcess)
