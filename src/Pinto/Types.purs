module Pinto.Types where

import Erl.Atom (Atom)
import Erl.Data.Tuple (Tuple2)
import Erl.Process.Raw (Pid)

-- | Defines the server name for a gen server, along with the 'state' that the gen server
-- | will be using internally - this will be supplied to every call to the gen server API in order
-- | to enforce type safety across calls
data ServerName state = ServerName String

-- | The result of invoking gen_server:start_link
type StartLinkResult = (Tuple2 Atom Pid)
  
-- | The result of supervisor:start_child
data StartChildResult = AlreadyStarted Pid | Started Pid
