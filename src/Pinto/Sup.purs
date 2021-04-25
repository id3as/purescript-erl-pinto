module Pinto.Sup
  ( ChildShutdownTimeoutStrategy(..)
  , ChildSpec(..)
  , ChildType(..)
  , ChildNotStartedReason(..)
  , StartChildResult(..)
  , ErlChildSpec
  , Flags
  , RestartStrategy(..)
  , Strategy(..)
  , SupervisorSpec
  , SupervisorRef(..)
  , SupervisorPid
  , SupervisorType
  , Millisecond
  , Seconds
  , spec
  , startLink
  , stop
  , maybeChildStarted
  , maybeChildRunning
  , crashIfChildNotStarted
  , crashIfChildNotRunning
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Process.Raw (Pid, class HasPid)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Pinto.Types (RegistryName, StartLinkResult)

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

-- maps to transient | permanent | temporary
data RestartStrategy
  = RestartNever
  | RestartAlways
  | RestartOnCrash

type Millisecond
  = Int

type Seconds
  = Int

data ChildShutdownTimeoutStrategy
  = KillImmediately -- brutal
  | KillNever -- infinity
  | KillAfter Millisecond -- {timeout, non_neg_integer()}

data ChildType
  = Supervisor
  | Worker

type ChildSpec childProcess
  = { id :: String
    , start :: Effect (StartLinkResult childProcess)
    , restartStrategy :: RestartStrategy
    , shutdownStrategy :: ChildShutdownTimeoutStrategy
    , childType :: ChildType
    }

data Strategy
  = OneForAll
  | OneForOne
  | RestForOne

type Flags
  = { strategy :: Strategy
    , intensity :: Int
    , period :: Seconds
    }

newtype SupervisorType
  = SupervisorType Void

newtype SupervisorPid
  = SupervisorPid Pid

derive newtype instance supervisorPidHasPid :: HasPid SupervisorPid

data SupervisorRef
  = ByName (RegistryName SupervisorType)
  | ByPid SupervisorPid

type SupervisorSpec
  = { flags :: Flags
    , childSpecs :: List ErlChildSpec
    }

foreign import startLink ::
  Maybe (RegistryName SupervisorType) ->
  Effect SupervisorSpec ->
  Effect (StartLinkResult SupervisorPid)

foreign import stop :: SupervisorRef -> Effect Unit

foreign import data ErlChildSpec :: Type

foreign import specFFI ::
  forall childProcess.
  ChildSpec childProcess ->
  ErlChildSpec

spec ::
  forall childProcess.
  HasPid childProcess =>
  ChildSpec childProcess ->
  ErlChildSpec
spec = specFFI

foreign import startChildFFI ::
  forall childProcess.
  SupervisorRef ->
  ChildSpec childProcess ->
  StartChildResult childProcess

startChild ::
  forall childProcess.
  HasPid childProcess =>
  SupervisorRef ->
  ChildSpec childProcess ->
  StartChildResult childProcess
startChild = startChildFFI

maybeChildStarted :: forall childProcess. StartChildResult childProcess -> Maybe childProcess
maybeChildStarted slr = case slr of
  ChildStarted { pid: childProcess } -> Just childProcess
  _ -> Nothing

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
