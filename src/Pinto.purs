module Pinto
  ( isRegistered
  , node
  , self
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
import Pinto.Types (class HasProcess, class HasRawPid, NotStartedReason(..), RegistryName(..), StartLinkResult, TerminateReason(..), crashIfNotRunning, crashIfNotStarted, getProcess, getRawPid, maybeRunning, maybeStarted, startLinkResultFromPs) as PintoTypeExports

foreign import node :: Effect String

foreign import self :: Effect Pid

foreign import isRegisteredImpl :: Foreign -> Effect Boolean

foreign import alreadyStartedImpl :: Foreign -> Effect Pid

isRegistered :: forall serverType. RegistryName serverType -> Effect Boolean
isRegistered (Local name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "local") name

isRegistered (Global name) = isRegisteredImpl $ unsafeToForeign $ tuple2 (atom "global") name

isRegistered (Via (NativeModuleName m) name) = isRegisteredImpl $ unsafeToForeign $ tuple3 (atom "via") m name
