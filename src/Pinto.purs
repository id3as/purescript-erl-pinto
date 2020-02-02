module Pinto ( isRegistered
             , ok
             , okAlreadyStarted
             , module PintoTypeExports
             )
where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process.Raw (Pid)
import Foreign (Foreign, unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Pinto.Types (ServerName(..), StartLinkResult)

import Pinto.Types (ChildTemplate(..), ServerName(..), StartLinkResult, SupervisorName) as PintoTypeExports



foreign import isRegisteredImpl :: Foreign -> Effect Boolean
foreign import alreadyStartedImpl :: Foreign -> Effect Pid


isRegistered :: forall a b. ServerName a b -> Effect Boolean
isRegistered (Local name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "local") name
isRegistered (Global name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "global") name
isRegistered (Via (NativeModuleName m) name) = isRegisteredImpl $ unsafeToForeign $ tuple3 (atom "via") m name


ok :: StartLinkResult -> Effect Pid
ok either = unsafePartial $
  case either of Right pid -> pure pid

okAlreadyStarted :: StartLinkResult -> Effect Pid
okAlreadyStarted (Right pid) = pure pid
okAlreadyStarted (Left l) = alreadyStartedImpl l
