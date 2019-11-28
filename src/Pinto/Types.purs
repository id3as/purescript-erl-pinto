module Pinto.Types where

import Erl.Atom (Atom)
import Erl.Data.Tuple (Tuple2)
import Erl.Process.Raw (Pid)
import Effect (Effect)
import Erl.ModuleName (NativeModuleName)
import Foreign (Foreign)

-- | Defines the server name for a gen server, along with the 'state' that the gen server
-- | will be using internally and the 'msg' type that will be received in the handleInfo calls
-- | this will be supplied to every call to the gen server API in order
-- | to enforce type safety across calls
data ServerName state msg = Local String
                          | Global String
                          | Via NativeModuleName Foreign

-- | The result of invoking gen_server:start_link
type StartLinkResult = (Tuple2 Atom Pid)
  
-- | The result of supervisor:start_child
data StartChildResult = AlreadyStarted Pid | Started Pid

-- | The type used to link startSimpleChild and startTemplate together
data ChildTemplate args = ChildTemplate (args -> Effect StartLinkResult)
