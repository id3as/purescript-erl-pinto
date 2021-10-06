-- | The base Pinto module re-exports most of the library's useful types and thats about it
module Pinto
  ( isRegistered
  , node
  , module PintoTypeExports
  ) where

import Prelude
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process.Raw (Pid)
import Foreign (Foreign, unsafeToForeign)
import Pinto.Types (RegistryName(..))
import Pinto.Types (NotStartedReason(..), RegistryName(..), RegistryReference(..), StartLinkResult, TerminateReason(..), crashIfNotRunning, crashIfNotStarted, maybeRunning, maybeStarted, startLinkResultFromPs) as PintoTypeExports

foreign import isRegisteredImpl :: Foreign -> Effect Boolean

foreign import alreadyStartedImpl :: Foreign -> Effect Pid

foreign import node :: Effect String

-- | Checks if a particular process name is registered using 'whereis_name'
-- |
-- | - `serverType` can be anything, but most likely it'll be GenServer.ServerType or GenStatem.ServerType
isRegistered :: forall serverType. RegistryName serverType -> Effect Boolean
isRegistered (Local name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "local") name

isRegistered (Global name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "global") name

isRegistered (Via (NativeModuleName m) name) = isRegisteredImpl $ unsafeToForeign $ tuple3 (atom "via") m name
