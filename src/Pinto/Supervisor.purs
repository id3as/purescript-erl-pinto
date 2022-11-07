-- | This module represents supervisor in OTP
-- | See also gen_supervisor in the OTP docs (https://erlang.org/doc/man/supervisor.html#)
module Pinto.Supervisor
  ( ChildShutdownTimeoutStrategy(..)
  , ChildSpec(..)
  , ChildType(..)
  , ChildNotStartedReason(..)
  , StartChildResult(..)
  , ErlChildSpec
  , Flags
  , RestartStrategy(..)
  , StopTimeoutStrategy(..)
  , Strategy(..)
  , SupervisorSpec
  , SupervisorRef(..)
  , SupervisorPid
  , SupervisorType
  , TerminateChildResult(..)
  , DeleteChildResult(..)
  , spec
  , startLink
  , startChild
  , terminateChild
  , deleteChild
  , stop
  , maybeChildStarted
  , maybeChildRunning
  , crashIfChildNotStarted
  , crashIfChildNotRunning
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds, Seconds)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Process.Raw (Pid, class HasPid)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Pinto.Types (RegistryInstance, RegistryName, RegistryReference, StartLinkResult, registryInstance)

data ChildNotStartedReason :: Type -> Type
data ChildNotStartedReason childProcess

data StartChildResult childProcess
  = ChildAlreadyPresent
  | ChildAlreadyStarted childProcess
  | ChildStartReturnedIgnore
  | ChildFailed Foreign
  | ChildStarted
      { pid :: childProcess
      , info :: Maybe Foreign
      }

data DeleteChildResult
  = ChildDeleted
  | ChildNotFoundToDelete

data TerminateChildResult
  = ChildTerminated
  | ChildNotFoundToTerminate

-- | This maps to transient | permanent | temporary as per the underlying supervisor API
data RestartStrategy
  = RestartTransient
  | RestartPermanent
  | RestartTemporary

-- | This maps to brutal | infinity | { timeout, Milliseconds } as per the underlying supervisor API
data ChildShutdownTimeoutStrategy
  = ShutdownBrutal
  | ShutdownInfinity
  | ShutdownTimeout Milliseconds

-- | This maps to supervisor | worker as per the underlying supervisor API
data ChildType
  = Supervisor
  | Worker

-- | The specification of a child process, this maps to ChildSpec in the underlying supervisor API
-- | with the difference that none of them are optional.
-- |
-- | `childProcess` is the typed pid of the started process (commonly GenServer.ServerPid)
type ChildSpec childProcess =
  { id :: String
  , start :: Effect (StartLinkResult childProcess)
  , restartStrategy :: RestartStrategy
  , shutdownStrategy :: ChildShutdownTimeoutStrategy
  , childType :: ChildType
  }

-- | The strategy employed by this supervision tree, this maps to
-- | one_for_all | one_for_one | rest_for_one in the underlying supervisor API
-- |
-- | Note: simple_one_for_one is deliberately missing, see: `Supervisor.SimpleOneForOne`
data Strategy
  = OneForAll
  | OneForOne
  | RestForOne

-- | The flags for a supervision tree, mapping onto sup_flags from the underlying supervisor API
type Flags =
  { strategy :: Strategy
  , intensity :: Int
  , period :: Seconds
  }

newtype SupervisorType = SupervisorType Void

newtype SupervisorPid = SupervisorPid Pid

derive newtype instance supervisorPidHasPid :: HasPid SupervisorPid

type SupervisorRef = RegistryReference SupervisorPid SupervisorType

type SupervisorInstance = RegistryInstance SupervisorPid SupervisorType

-- | A complete specification for a supervisor, see `Result` in the underlying supervisor API
type SupervisorSpec =
  { flags :: Flags
  , childSpecs :: List ErlChildSpec
  }

-- | Given an (optional) name for the supervisor
-- | an effect that will be executed within the context of the new supervisor
-- | execute that effect to get the specification and start a supervisor with that specification
foreign import startLink
  :: Maybe (RegistryName SupervisorType)
  -> Effect SupervisorSpec
  -> Effect (StartLinkResult SupervisorPid)

-- | A convenience mechanism to stop a supervisor using sys:terminate
data StopTimeoutStrategy
  = StopInfinity
  | StopTimeout Milliseconds

foreign import stopFFI :: StopTimeoutStrategy -> SupervisorInstance -> Effect Unit

stop :: StopTimeoutStrategy -> SupervisorRef -> Effect Unit
stop timeout = registryInstance >>> stopFFI timeout

foreign import data ErlChildSpec :: Type

foreign import specFFI
  :: forall childProcess
   . ChildSpec childProcess
  -> ErlChildSpec

spec
  :: forall childProcess
   . HasPid childProcess
  => ChildSpec childProcess
  -> ErlChildSpec
spec = specFFI

foreign import startChildFFI
  :: forall childProcess
   . SupervisorInstance
  -> ChildSpec childProcess
  -> StartChildResult childProcess

-- | Given a supervisor reference and a child specification
-- | start a new child within the context of that supervisor
-- | See also supervisor:start_child
startChild
  :: forall childProcess
   . HasPid childProcess
  => SupervisorRef
  -> ChildSpec childProcess
  -> StartChildResult childProcess
startChild r = startChildFFI $ registryInstance r

foreign import terminateChildFFI :: SupervisorInstance -> String -> Effect TerminateChildResult
foreign import deleteChildFFI :: SupervisorInstance -> String -> Effect DeleteChildResult

-- | Given a supervisor reference and a child id
-- | terminate that given child
-- | See also supervisor:terminate_child
terminateChild :: SupervisorRef -> String -> Effect TerminateChildResult
terminateChild r id = terminateChildFFI (registryInstance r) id

-- | Given a supervisor reference and a child id
-- | delete that given child
-- | See also supervisor:delete_child
deleteChild :: SupervisorRef -> String -> Effect DeleteChildResult
deleteChild r id = deleteChildFFI (registryInstance r) id

-- | Converts a StartChildResult into a Maybe
-- | returning Just if the child was started
-- | and Nothing otherwise
maybeChildStarted :: forall childProcess. StartChildResult childProcess -> Maybe childProcess
maybeChildStarted slr = case slr of
  ChildStarted { pid: childProcess } -> Just childProcess
  _ -> Nothing

-- | Converts a StartChildResult into a Maybe
-- | returning Just if the child was started
-- | and Just if the child was already running
-- | and Nothing otherwise
maybeChildRunning :: forall childProcess. StartChildResult childProcess -> Maybe childProcess
maybeChildRunning slr = case slr of
  ChildStarted { pid: childProcess } -> Just childProcess
  (ChildAlreadyStarted childProcess) -> Just childProcess
  _ -> Nothing

crashIfChildNotStarted :: forall childProcess. StartChildResult childProcess -> childProcess
crashIfChildNotStarted =
  unsafePartial \slr -> case maybeChildStarted slr of
    Just childProcess -> childProcess

crashIfChildNotRunning :: forall childProcess. StartChildResult childProcess -> childProcess
crashIfChildNotRunning =
  unsafePartial \slr -> case maybeChildRunning slr of
    Just childProcess -> childProcess
