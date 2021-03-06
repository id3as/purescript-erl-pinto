module Pinto ( isRegistered
             , ok
             , ok'
             , okAlreadyStarted
             , okAlreadyStarted'
             , node
             , self
             , module PintoTypeExports
             )
where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process.Raw (Pid)
import Foreign (Foreign, unsafeToForeign)

import Partial.Unsafe (unsafePartial)
import Pinto.Types (ChildTemplate(..), ServerName(..), StartLinkResult(..), StartChildResult(..), SupervisorName, TerminateReason(..)) as PintoTypeExports
import Pinto.Types (class StartOk, ServerName(..), startOk, startOkAS)

foreign import node :: Effect String
foreign import self :: Effect Pid
foreign import isRegisteredImpl :: Foreign -> Effect Boolean
foreign import alreadyStartedImpl :: Foreign -> Effect Pid

isRegistered :: forall a b. ServerName a b -> Effect Boolean
isRegistered (Local name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "local") name
isRegistered (Global name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "global") name
isRegistered (Via (NativeModuleName m) name) = isRegisteredImpl $ unsafeToForeign $ tuple3 (atom "via") m name

ok' :: forall r. StartOk r => r -> Effect Pid
ok' r = unsafePartial $
  case (startOk r) of Just pid -> pure pid

okAlreadyStarted' :: forall r. StartOk r => r -> Effect Pid
okAlreadyStarted' r = unsafePartial $
  case (startOkAS r) of Just pid -> pure pid

ok :: forall r. StartOk r => r -> Effect Unit
ok = void <<< ok'

okAlreadyStarted :: forall r. StartOk r => r -> Effect Unit
okAlreadyStarted = void <<< okAlreadyStarted'
