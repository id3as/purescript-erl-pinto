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

  , Millisecond
  , Seconds

  , mkErlChildSpec
  , startLink
 ) where


import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.List (List)
import Foreign (Foreign)
import Pinto.Types (InstanceRef, RegistryName, StartLinkResult)

type ChildStarted childType
  = { pid :: childType
    , info :: Maybe Foreign
    }

data ChildNotStartedReason childType
  = ChildAlreadyPresent
  | ChildAlreadyStarted childType
  | ChildStartReturnedIgnore
  | ChildFailed Foreign


type StartChildResult childType
  = Either (ChildNotStartedReason childType) (ChildStarted childType)

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

type ChildSpec serverType
  = { id :: String
    , start :: Effect (StartLinkResult serverType)
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

type SupervisorSpec
  = { flags :: Flags
    , childSpecs :: List ErlChildSpec
    }

foreign import startLink :: forall supType. Maybe (RegistryName supType) -> Effect SupervisorSpec -> Effect (StartLinkResult supType)

foreign import data ErlChildSpec :: Type
foreign import mkErlChildSpec :: forall serverType. ChildSpec serverType -> ErlChildSpec

foreign import startChild :: forall supType childServerType. InstanceRef supType -> ChildSpec childServerType -> StartChildResult childServerType
