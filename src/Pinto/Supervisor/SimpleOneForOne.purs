-- | This module represents a simple_one_for_one supervisor in OTP
-- | It is a special case because if it has different return values and arguments for pretty much every function
-- | See also gen_supervisor in the OTP docs (https://erlang.org/doc/man/supervisor.html#)
module Pinto.Supervisor.SimpleOneForOne
  ( ChildSpec
  , SupervisorRef(..)
  , SupervisorPid
  , SupervisorType
  , startLink
  , startChild
  , terminateChild
  , deleteChild
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds)
import Effect (Effect)
import Erl.Process.Raw (Pid, class HasPid)
import Pinto.Supervisor (ChildShutdownTimeoutStrategy, ChildType, RestartStrategy, StartChildResult, TerminateChildResult, DeleteChildResult)
import Pinto.Types (RegistryInstance, RegistryName, RegistryReference, StartLinkResult, registryInstance)

newtype SupervisorType :: Type -> Type -> Type
newtype SupervisorType childStartArg childProcess
  = SupervisorType Void

newtype SupervisorPid :: Type -> Type -> Type
newtype SupervisorPid childStartArg childProcess
  = SupervisorPid Pid

derive newtype instance supervisorPidHasPid :: HasPid (SupervisorPid childStartArg childProcess)

type SupervisorRef childStartArg childProcess
  = RegistryReference (SupervisorPid childStartArg childProcess) (SupervisorType childStartArg childProcess)

type SupervisorInstance childStartArg childProcess
  = RegistryInstance (SupervisorPid childStartArg childProcess) (SupervisorType childStartArg childProcess)

-- | The specification of the dynamic child process, this maps to ChildSpec in the underlying supervisor API
-- | with the difference that none of them are optional
-- | `childStartArg` is the parameter expected by the start function for the child. It is provided here
-- | `childProcess` is the typed pid of the started process (commonly GenServer.ServerPid)
type ChildSpec childStartArg childProcess
  = { intensity :: Int
    , period :: Seconds
    , start :: childStartArg -> Effect (StartLinkResult childProcess)
    , restartStrategy :: RestartStrategy
    , shutdownStrategy :: ChildShutdownTimeoutStrategy
    , childType :: ChildType
    }

-- | Start the supervisor, running the provided Effect to get the spec of the children that this OneForOne supervisor can start
-- | Returns a pid that can be used to communicate with the supervisor in the startChild call
startLink ::
  forall childStartArg childProcess.
  HasPid childProcess =>
  Maybe (RegistryName (SupervisorType childStartArg childProcess)) ->
  Effect (ChildSpec childStartArg childProcess) ->
  Effect (StartLinkResult (SupervisorPid childStartArg childProcess))
startLink = startLinkFFI

foreign import startLinkFFI ::
  forall childStartArg childProcess.
  Maybe (RegistryName (SupervisorType childStartArg childProcess)) ->
  Effect (ChildSpec childStartArg childProcess) ->
  Effect (StartLinkResult (SupervisorPid childStartArg childProcess))

-- | Starts a child within th one_for_one supervisor referred to by `SupervisorRef childStartArg childProcess`
-- | See also supervisor:start_child in the OTP documentation
startChild ::
  forall childStartArg childProcess.
  HasPid childProcess =>
  SupervisorRef childStartArg childProcess ->
  childStartArg ->
  Effect (StartChildResult childProcess)
startChild = startChildFFI <<< registryInstance

foreign import startChildFFI ::
  forall childStartArg childProcess.
  SupervisorInstance childStartArg childProcess ->
  childStartArg ->
  Effect (StartChildResult childProcess)

foreign import terminateChildFFI :: forall childStartArg childProcess. SupervisorInstance childStartArg childProcess -> childProcess -> Effect TerminateChildResult
foreign import deleteChildFFI :: forall childStartArg childProcess. SupervisorInstance childStartArg childProcess -> childProcess -> Effect DeleteChildResult

-- | Terminates a child within the one_for_one supervisor referred to by `SupervisorRef childStartArg childProcess`
-- | See also supervisor:terminate_child in the OTP documentation
terminateChild :: forall childStartArg childProcess. SupervisorRef childStartArg childProcess -> childProcess -> Effect TerminateChildResult
terminateChild r p = terminateChildFFI (registryInstance r) p

-- | Deletes a child within the one_for_one supervisor referred to by `SupervisorRef childStartArg childProcess`
-- | See also supervisor:deleted_child in the OTP documentation
deleteChild :: forall childStartArg childProcess. SupervisorRef childStartArg childProcess -> childProcess -> Effect DeleteChildResult
deleteChild r p = deleteChildFFI (registryInstance r) p
