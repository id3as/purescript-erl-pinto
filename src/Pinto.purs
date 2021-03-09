module Pinto ( isRegistered
             -- , ok
             -- , ok'
             -- , okAlreadyStarted
             -- , okAlreadyStarted'
             , node

             , maybeStarted
             , maybeRunning

             , crashIfNotStarted
             , crashIfNotRunning

             , module PintoTypeExports
             )
where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process.Raw (Pid)
import Foreign (Foreign, unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Pinto.Types (RegistryName(..), StartLinkResult, NotStartedReason(..))
-- import Pinto.Types (class StartOk, ServerName(..), startOk, startOkAS)
import Pinto.Types as PintoTypeExports

foreign import node :: Effect String
foreign import isRegisteredImpl :: Foreign -> Effect Boolean
foreign import alreadyStartedImpl :: Foreign -> Effect Pid

isRegistered :: forall serverType. RegistryName serverType -> Effect Boolean
isRegistered (Local name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "local") name
isRegistered (Global name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "global") name
isRegistered (Via (NativeModuleName m) name) = isRegisteredImpl $ unsafeToForeign $ tuple3 (atom "via") m name

maybeStarted :: forall serverProcess. StartLinkResult serverProcess -> Maybe serverProcess
maybeStarted slr = case slr of
    Right serverProcess -> Just serverProcess
    _ -> Nothing

maybeRunning :: forall serverProcess. StartLinkResult serverProcess -> Maybe serverProcess
maybeRunning slr = case slr of
    Right serverProcess -> Just serverProcess
    Left (AlreadyStarted serverProcess) -> Just serverProcess
    _ -> Nothing


crashIfNotStarted :: forall serverProcess. StartLinkResult serverProcess -> serverProcess
crashIfNotStarted = unsafePartial \slr ->
  case maybeStarted slr of
     Just serverProcess -> serverProcess

crashIfNotRunning :: forall serverProcess. StartLinkResult serverProcess -> serverProcess
crashIfNotRunning = unsafePartial \slr ->
  case maybeRunning slr of
     Just serverProcess -> serverProcess

-- ok' :: forall r. StartOk r => r -> Effect Pid
-- ok' r = unsafePartial $
--   case (startOk r) of Just pid -> pure pid

-- okAlreadyStarted' :: forall r. StartOk r => r -> Effect Pid
-- okAlreadyStarted' r = unsafePartial $
--   case (startOkAS r) of Just pid -> pure pid

-- ok :: forall r. StartOk r => r -> Effect Unit
-- ok = void <<< ok'

-- okAlreadyStarted :: forall r. StartOk r => r -> Effect Unit
-- okAlreadyStarted = void <<< okAlreadyStarted'
