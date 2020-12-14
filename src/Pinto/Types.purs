module Pinto.Types
       ( -- Names and handles
         RegistryName(..)
         -- Result Types -- TODO - move these to Gen and Sup?
       , TerminateReason(..)
       , StartLinkResult(..)
       , NotStartedReason(..)
       , Handle(..)
         -- Opaque types
       , ServerPid
       , GlobalName

       -- , class StartOk
       -- , startOk
       -- , startOkAS
       )
       where

import Data.Either (Either)
import Erl.Atom (Atom)
import Erl.ModuleName (NativeModuleName)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)


foreign import data GlobalName :: Type

-- | Defines the server name for a gen server, along with the 'state' that the gen server
-- | will be using internally and the 'msg' type that will be received in the handleInfo calls
-- | this will be supplied to every call to the gen server API in order
-- | to enforce type safety across calls



data RegistryName state msg
  = Local Atom
  | Global GlobalName
  | Via NativeModuleName Foreign

newtype ServerPid state msg = ServerPid Pid

data Handle state msg
  = ByName (RegistryName state msg)
  | ByPid (ServerPid state msg)


data NotStartedReason state msg
  = Ignore
  | AlreadyStarted (ServerPid state msg)
  | Failed Foreign


type StartLinkResult state msg
  = Either (NotStartedReason state msg) (ServerPid state msg)




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
