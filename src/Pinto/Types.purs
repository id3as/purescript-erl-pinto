module Pinto.Types
       ( ServerName(..)
       , GlobalName
       , SupervisorName
       , StartLinkResult(..)
       , StartChildResult(..)
       , ChildTemplate(..)
       , TerminateReason(..)
       , class StartOk
       , startOk
       , startOkAS
       )
       where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.ModuleName (NativeModuleName)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Prelude (Unit)


foreign import data GlobalName :: Type

-- | Defines the server name for a gen server, along with the 'state' that the gen server
-- | will be using internally and the 'msg' type that will be received in the handleInfo calls
-- | this will be supplied to every call to the gen server API in order
-- | to enforce type safety across calls
data ServerName state msg = Local Atom
                          | Global GlobalName
                          | Via NativeModuleName Foreign

type SupervisorName = ServerName Unit Unit

-- | The result of invoking gen_server:start_link
data StartLinkResult
  = Ok Pid
  | Ignore
  | AlreadyStarted Pid
  | Failed Foreign

data TerminateReason
  = Normal
  | Shutdown
  | ShutdownWithCustom Foreign
  | Custom Foreign

-- | The result of invoking gen_server:start_link
data StartChildResult
  = ChildStarted Pid
  | ChildStartedWithInfo Pid Foreign
  | ChildAlreadyStarted Pid
  | ChildAlreadyPresent
  | ChildFailed Foreign

-- | The type used to link startSimpleChild and startTemplate together
data ChildTemplate args = ChildTemplate (args -> Effect StartLinkResult)

class StartOk a where
  startOk :: a -> Maybe Pid
  startOkAS :: a -> Maybe Pid

instance startLinkResultOk :: StartOk StartLinkResult where
  startOk (Ok p) = Just p
  startOk _ = Nothing

  startOkAS (Ok p) = Just p
  startOkAS (AlreadyStarted p) = Just p
  startOkAS _ = Nothing

instance startChildResultOk :: StartOk StartChildResult where
  startOk (ChildStarted p) = Just p
  startOk (ChildStartedWithInfo p _) = Just p
  startOk _ = Nothing

  startOkAS (ChildStarted p) = Just p
  startOkAS (ChildStartedWithInfo p _) = Just p
  startOkAS (ChildAlreadyStarted p) = Just p
  startOkAS _ = Nothing
