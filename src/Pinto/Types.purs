module Pinto.Types where

import Data.Either (Either)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.ModuleName (NativeModuleName)
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Prelude (Unit)

-- | Defines the server name for a gen server, along with the 'state' that the gen server
-- | will be using internally and the 'msg' type that will be received in the handleInfo calls
-- | this will be supplied to every call to the gen server API in order
-- | to enforce type safety across calls
data ServerName state msg = Local Atom
                          | Global Atom
                          | Via NativeModuleName Foreign

type SupervisorName = ServerName Unit Unit

-- | The result of invoking gen_server:start_link
type StartLinkResult = Either Foreign Pid

-- | The result of supervisor:start_child
data StartChildResult = AlreadyStarted Pid | Started Pid

-- | The type used to link startSimpleChild and startTemplate together
data ChildTemplate args = ChildTemplate (args -> Effect StartLinkResult)
