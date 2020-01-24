module Pinto ( isRegistered
             , module Pinto.Types
             )
where

import Prelude

import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (Foreign, unsafeToForeign)
import Pinto.Types (ServerName(..), SupervisorName, StartChildResult(..), StartLinkResult, ChildTemplate(..))


foreign import isRegisteredImpl :: forall a b. Foreign -> Effect Boolean


isRegistered :: forall a b. ServerName a b -> Effect Boolean
isRegistered (Local name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "local") name
isRegistered (Global name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "global") name
isRegistered (Via (NativeModuleName m) name) = isRegisteredImpl $ unsafeToForeign $ tuple3 (atom "via") m name
