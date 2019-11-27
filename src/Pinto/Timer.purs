-- | This module provides a means of using the timer functionality in core Erlang
-- | This is specifically for use within the context of gen-servers and will not work otherwise
module Pinto.Timer 
  ( sendEvery,
    sendAfter,
    cancel,
    TimerRef ) 
    where

import Prelude

import Effect (Effect)
import Erl.Process.Raw (Pid)
import Pinto (ServerName)

newtype TimerRef = TimerRef Pid

foreign import sendEvery_ :: forall msg.  (Pid -> TimerRef) -> Int -> msg -> Effect TimerRef
foreign import sendAfter_ :: forall msg.  (Pid -> TimerRef) -> Int -> msg -> Effect TimerRef
foreign import cancel_ :: Pid -> Effect Unit

-- | Sends the supplied message back to the recipient every N milliseconds
-- | See also timer:send_every in the OTP docs
sendEvery :: forall state msg. ServerName state msg -> Int -> msg -> Effect TimerRef
sendEvery _ = sendEvery_ TimerRef

-- | Sends the supplied message back to the recipient after N milliseconds
-- | See also timer:send_after in the OTP docs
sendAfter :: forall state msg. ServerName state msg -> Int -> msg -> Effect TimerRef
sendAfter _ = sendAfter_ TimerRef

-- | Cancels a timer started by either sendEvery or sendAfter
-- | See also timer:cancel in the OTP docs
cancel :: TimerRef -> Effect Unit
cancel (TimerRef pid) = cancel_ pid
