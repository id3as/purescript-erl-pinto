module Pinto.Types
  ( TerminateReason(..)
  , RegistryName(..)
  , StartLinkResult
  , NotStartedReason(..)
  , maybeStarted
  , maybeRunning
  , crashIfNotStarted
  , crashIfNotRunning
  , startLinkResultFromPs
  , registryInstance
  , RegistryInstance
  , RegistryReference(..)
  , ExitMessage(..)
  , ShutdownReason(..)
  , parseShutdownReasonFFI
  , parseTrappedExitFFI
  , class ExportsTo
  , export
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Erl.Atom (Atom)
import Erl.ModuleName (NativeModuleName)
import Erl.Process.Raw (class HasPid, Pid, getPid)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | The name of a registered process, these map to
-- | `{local, Name}`
-- | `{global, GlobalName}`
-- | `{via, Module, ViaName}`
-- | as per the docs for gen_server:start_link and similar
data RegistryName :: Type -> Type
data RegistryName serverType
  = Local Atom
  | Global Foreign
  | Via NativeModuleName Foreign

-- | A means of looking up a typed process (such as a GenServer)
-- | that may or may not be registered
-- |
-- | This is typically used by the APIs provided to gain access
-- | to the ability to invoke code within the context of a started server
-- |
-- | ```purescript
-- | GenServer.call (ByName serverName) \_from a...
-- | ```
data RegistryReference :: Type -> Type -> Type
data RegistryReference serverPid serverType
  = ByPid serverPid
  | ByName (RegistryName serverType)

foreign import data RegistryInstance :: Type -> Type -> Type

-- | Given a `RegistryReference serverPid serverType`
-- |
-- | Create a `RegistryInstance serverPid serverType` that can be used
-- | to communicate with that process directly
registryInstance ::
  forall serverPid serverType.
  HasPid serverPid =>
  RegistryReference serverPid serverType -> RegistryInstance serverPid serverType
registryInstance (ByPid pid) = registryPidToInstance pid

registryInstance (ByName name) = registryNameToInstance name

registryPidToInstance :: forall serverPid serverType. HasPid serverPid => serverPid -> RegistryInstance serverPid serverType
registryPidToInstance serverPid = unsafeCoerce $ getPid serverPid

registryNameToInstance :: forall serverPid serverType. RegistryName serverType -> RegistryInstance serverPid serverType
registryNameToInstance (Local atom) = unsafeCoerce atom

registryNameToInstance other = unsafeCoerce other

data ExitMessage
  = Exit Pid Foreign

data ShutdownReason
  = ReasonNormal
  | ReasonShutdown (Maybe Foreign)
  | ReasonOther Foreign

foreign import parseTrappedExitFFI :: Foreign -> (Pid -> Foreign -> ExitMessage) -> Maybe ExitMessage

foreign import parseShutdownReasonFFI :: Foreign -> ShutdownReason

data NotStartedReason serverProcess
  = Ignore
  | AlreadyStarted serverProcess
  | Failed Foreign

derive instance Functor NotStartedReason

instance (Show serverProcess) => Show (NotStartedReason serverProcess) where
  show Ignore = "Ignore"
  show (AlreadyStarted process) = "(AlreadyStarted " <> show process <> ")"
  show (Failed _) = "Failed (foreign)"

type StartLinkResult serverProcess
  = Either (NotStartedReason serverProcess) serverProcess

data TerminateReason
  = Normal
  | Shutdown
  | ShutdownWithCustom Foreign
  | Custom Foreign

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
crashIfNotStarted =
  unsafePartial \slr -> case maybeStarted slr of
    Just serverProcess -> serverProcess

crashIfNotRunning :: forall serverProcess. StartLinkResult serverProcess -> serverProcess
crashIfNotRunning =
  unsafePartial \slr -> case maybeRunning slr of
    Just serverProcess -> serverProcess

startLinkResultFromPs :: forall a. StartLinkResult a -> Foreign
startLinkResultFromPs = start_link_result_from_ps

foreign import start_link_result_from_ps :: forall a. StartLinkResult a -> Foreign

class ExportsTo a b where
  export :: a -> b
