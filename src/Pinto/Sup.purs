module Pinto.Sup (
  StartName()
  ) where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Data.List as List
import Erl.Data.Tuple (Tuple2, Tuple3, tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process.Raw (Pid)
import Foreign (Foreign, unsafeToForeign)
import Pinto as Pinto
import Pinto.Gen as Gen
import Pinto.Types (RegistryName(..), ServerPid, StartLinkResult)
import Pinto.Types as Types
import Unsafe.Coerce (unsafeCoerce)


type StartName a
  = Maybe (RegistryName a Void)

data Handle a
  = NamedSupervisorHandle (RegistryName a Void)
  | AnonymousSupervisorHandle (ServerPid a Void)

data ChildStarted state msg
  = ChildStarted (ServerPid state msg)
  | ChildStartedWithInfo (ServerPid state msg) Foreign

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

data ChildShutdownTimeoutStrategy
  = KillImmediately       -- brutal
  | KillNever             -- infinity
  | KillAfter Millisecond -- {timeout, non_neg_integer()}


data ChildType
  = Supervisor
  | Worker

newtype ChildId id state msg = ChildId id

type ChildSpec id state msg
  = { id :: ChildId id state msg
    , start :: Effect (StartLinkResult state msg)
    , restartStrategy :: RestartStrategy
    , shutdownStrategy :: ChildShutdownTimeoutStrategy
    , childType :: ChildType
    }

foreign import data ErlChildSpec :: Type
foreign import mkErlChildSpec :: forall id state ms. ChildSpec id state msg -> ErlChildSpec


data Strategy
  = OneForAll
  | OneForOne
  | RestForOne

type Flags
  = { strategy:: Strategy
    , intensity :: Int
    , period :: Int
    }

type SupervisorSpec
  = { flags :: Flags
    , childSpecs :: List ErlChildSpec
    }

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

--startLink :: forall supState. StartName supState Void -> List ErlChildSpec -> Effect (ServerPid supState Void)
foreign import startLInk :: forall supState. StartName supState Void -> Effect SupervisorSpec -> Effect (StartLinkResult supState Void)



mySup name  = startLink name children
  where
    children = existsChild myChild
             : existsChild myChild2
             : Nil


mkSup = unsafeCoerce unit




myChild :: forall childState childMsg. ChildSpec String childState childMsg
myChild = mkChildSpec
          { id: "myChildId"
          , start: myGenserverStartLink
          , restartStrategy: RestartOnCrash
          , shutdownTimeout: KillAfter 5000
          }


mkChildSpec = unsafeCoerce unit


myGenserverStartLink  = unsafeCoerce unit
