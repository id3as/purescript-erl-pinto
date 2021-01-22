module Pinto.Sup
  ( ChildId(..)
  , ChildStarted(..)
  , ChildShutdownTimeoutStrategy(..)
  , ChildSpec(..)
  , ChildType(..)
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

  , mkErlChildSpec
  , startLink
 ) where


import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Pinto.Types (RegistryName, StartLinkResult, class HasRawPid)

type ChildStarted childProcess
  = { pid :: childProcess
    , info :: Maybe Foreign
    }

data ChildNotStartedReason childProcess
  = ChildAlreadyPresent
  | ChildAlreadyStarted childProcess
  | ChildStartReturnedIgnore
  | ChildFailed Foreign


type StartChildResult childProcess
  = Either (ChildNotStartedReason childProcess) (ChildStarted childProcess)

-- maps to transient | permanent | temporary
data RestartStrategy = RestartNever | RestartAlways | RestartOnCrash


type Millisecond = Int
type Seconds = Int

data ChildShutdownTimeoutStrategy
  = KillImmediately       -- brutal
  | KillNever             -- infinity
  | KillAfter Millisecond -- {timeout, non_neg_integer()}


data ChildType
  = Supervisor
  | Worker

type ChildId id state msg = id

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
  = { strategy:: Strategy
    , intensity :: Int
    , period :: Seconds
    }

newtype SupervisorType = SupervisorType Void
newtype SupervisorPid = SupervisorPid Pid
derive newtype instance supervisorPidHasRawPid :: HasRawPid SupervisorPid

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

foreign import data ErlChildSpec :: Type
foreign import mkErlChildSpec ::
  forall childProcess.
  ChildSpec childProcess ->
  ErlChildSpec

-- TODO: this is just returning the type, not the actual pid...
-- TODO: should we do something with fundeps beween a process and its type?
foreign import startChild ::
  forall childProcess.
  SupervisorRef ->
  ChildSpec childProcess ->
  StartChildResult childProcess
