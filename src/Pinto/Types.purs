module Pinto.Types
  ( -- Names and handles
    RegistryName(..)

    -- Result Types -- TODO - move these to Gen and Sup?
  , TerminateReason(..)
  , StartLinkResult
  , NotStartedReason(..)

  , class HasRawPid
  , getRawPid
  , class HasProcess
  , getProcess

  , maybeStarted
  , maybeRunning

  , crashIfNotStarted
  , crashIfNotRunning

  , startLinkResultFromPs
  )
  where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Erl.Atom (Atom)
import Erl.ModuleName (NativeModuleName)
import Erl.Process (Process, toPid)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)


-- | Defines the server name for a gen server, along with the 'state' that the gen server
-- | will be using internally and the 'msg' type that will be received in the handleInfo calls
-- | this will be supplied to every call to the gen server API in order
-- | to enforce type safety across calls

data RegistryName :: Type -> Type
data RegistryName serverType
  = Local Atom
  | Global Foreign
  | Via NativeModuleName Foreign

class HasRawPid a where
  getRawPid :: a -> Pid

instance pidHasRawPid :: HasRawPid Pid where
  getRawPid = identity

instance processHasRawPid :: HasRawPid (Process b) where
  getRawPid = toPid

class HasProcess b a where
  getProcess :: a -> Process b

instance processHasProcess :: HasProcess b (Process b) where
  getProcess = identity

data NotStartedReason serverProcess
  = Ignore
  | AlreadyStarted serverProcess
  | Failed Foreign

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
crashIfNotStarted = unsafePartial \slr ->
  case maybeStarted slr of
     Just serverProcess -> serverProcess

crashIfNotRunning :: forall serverProcess. StartLinkResult serverProcess -> serverProcess
crashIfNotRunning = unsafePartial \slr ->
  case maybeRunning slr of
     Just serverProcess -> serverProcess

startLinkResultFromPs :: forall a. StartLinkResult a -> Foreign
startLinkResultFromPs = start_link_result_from_ps

foreign import start_link_result_from_ps :: forall a. StartLinkResult a -> Foreign

