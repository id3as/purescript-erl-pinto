module Pinto.Types
  ( -- Names and handles
    RegistryName(..)
    -- Result Types -- TODO - move these to Gen and Sup?
  , TerminateReason(..)
  , StartLinkResult(..)
  , NotStartedReason(..)
  , InstanceRef(..)
    -- Opaque types
  , ServerPid

  , maybeStarted
  , maybeRunning

  , crashIfNotStarted
  , crashIfNotRunning

  -- , class StartOk
  -- , startOk
  -- , startOkAS
  )
  where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Erl.Atom (Atom)
import Erl.ModuleName (NativeModuleName)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)


-- | Defines the server name for a gen server, along with the 'state' that the gen server
-- | will be using internally and the 'msg' type that will be received in the handleInfo calls
-- | this will be supplied to every call to the gen server API in order
-- | to enforce type safety across calls

data RegistryName serverType
  = Local Atom
  | Global Foreign
  | Via NativeModuleName Foreign


newtype ServerPid serverType = ServerPid Pid

data InstanceRef serverType
  = ByName (RegistryName serverType)
  | ByPid (ServerPid serverType)


data NotStartedReason serverType
  = Ignore
  | AlreadyStarted (ServerPid serverType)
  | Failed Foreign

type StartLinkResult serverType
  = Either (NotStartedReason serverType) (ServerPid serverType)

data TerminateReason
  = Normal
  | Shutdown
  | ShutdownWithCustom Foreign
  | Custom Foreign

maybeStarted :: forall serverType. StartLinkResult serverType -> Maybe (ServerPid serverType)
maybeStarted slr = case slr of
    Right serverType -> Just serverType
    _ -> Nothing

maybeRunning :: forall serverType. StartLinkResult serverType -> Maybe (ServerPid serverType)
maybeRunning slr = case slr of
    Right serverPid -> Just serverPid
    Left (AlreadyStarted serverPid) -> Just serverPid
    _ -> Nothing


crashIfNotStarted :: forall serverType. StartLinkResult serverType -> ServerPid serverType
crashIfNotStarted = unsafePartial \slr ->
  case maybeStarted slr of
     Just serverType -> serverType

crashIfNotRunning :: forall serverType. StartLinkResult serverType -> ServerPid serverType
crashIfNotRunning = unsafePartial \slr ->
  case maybeRunning slr of
     Just serverType -> serverType

-- class StartOk a state msg where
--   startOk :: a -> Maybe (ServerPid state msg)
--   startOkAS :: a -> Maybe (ServerPid state msg)

-- instance startLinkResultOk :: StartOk StartLinkResult where
--   startOk (Ok p) = Just p
--   startOk _ = Nothing

--   startOkAS (Ok p) = Just p
--   startOkAS (AlreadyStarted p) = Just p
--   startOkAS _ = Nothing

-- instance startChildResultOk :: StartOk StartChildResult where
--   startOk (ChildStarted p) = Just p
--   startOk (ChildStartedWithInfo p _) = Just p
--   startOk _ = Nothing

--   startOkAS (ChildStarted p) = Just p
--   startOkAS (ChildStartedWithInfo p _) = Just p
--   startOkAS (ChildAlreadyStarted p) = Just p
--   startOkAS _ = Nothing
