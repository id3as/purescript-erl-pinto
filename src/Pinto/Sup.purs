module Pinto.Sup (
  ChildStarted
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.List (List, nil, (:))
import Foreign (Foreign)
import Pinto.Types (Handle, RegistryName, ServerPid, StartLinkResult)
import Unsafe.Coerce (unsafeCoerce)


type ChildStarted state msg
  = { pid :: ServerPid state msg
    , info :: Maybe Foreign
    }

data ChildNotStartedReason state msg
  = ChildAlreadyPresent
  | ChildAlreadyStarted (ServerPid state msg)
  | ChildStartReturnedIgnore
  | ChildFailed Foreign


type StartChildResult state msg
  = Either (ChildNotStartedReason state msg) (ChildStarted state msg)


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

type ChildSpec id state msg
  = { id :: ChildId id state msg
    , start :: Effect (StartLinkResult state msg)
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

foreign import startLink :: forall supState. Maybe (RegistryName supState Void) -> Effect SupervisorSpec -> Effect (StartLinkResult supState Void)

foreign import data ErlChildSpec :: Type
foreign import mkErlChildSpec :: forall id state msg. ChildSpec id state msg -> ErlChildSpec

foreign import startChild :: forall supState childId childState childMsg. Handle supState Void -> ChildSpec childId childState childMsg -> StartChildResult childState childMsg

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

data SupStateExample = SupStateExample

mySup :: Maybe (RegistryName SupStateExample Void) -> Effect (StartLinkResult SupStateExample Void)
mySup name  = startLink name init
  where
    init = pure { flags: { strategy : OneForOne
                         , intensity : 1
                         , period: 5
                         }
                , childSpecs
                }
    childSpecs = mkErlChildSpec myChild
             : nil


myChild :: forall childState childMsg. ChildSpec String childState childMsg
myChild = (mkChildSpec "myChildId")
            { start = myGenserverStartLink
            }


mkChildSpec :: forall childState childMsg. String -> ChildSpec String childState childMsg
mkChildSpec id  = { id
                  , childType : Worker
                  , start : unsafeCoerce unit
                  , restartStrategy: RestartOnCrash
                  , shutdownStrategy: KillAfter 5000
                  }

myGenserverStartLink  = unsafeCoerce unit
