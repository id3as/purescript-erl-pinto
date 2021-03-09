module Pinto.Types
  ( RegistryName(..)
  , TerminateReason(..)
  , StartLinkResult(..)
  , NotStartedReason(..)

  -- , class StartOk
  -- , startOk
  -- , startOkAS
  )
  where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.ModuleName (NativeModuleName)
import Erl.Process (Process(..))
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
