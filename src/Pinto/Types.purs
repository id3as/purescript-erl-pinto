module Pinto.Types where

import Erl.Atom (Atom)
import Erl.Data.Tuple (Tuple2)
import Erl.Process.Raw (Pid)

data ServerName state = ServerName String
type StartLinkResult = (Tuple2 Atom Pid)
data StartChildResult = AlreadyStarted Pid | Started Pid
