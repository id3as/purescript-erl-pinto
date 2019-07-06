-- | This module provides a means of using the timer functionality in core Erlang
-- | This is subject to change as currently it only works within the context of a genserver
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

foreign import sendEvery_ :: (Pid -> TimerRef) -> Int -> Effect Unit -> Effect TimerRef
foreign import sendAfter_ :: (Pid -> TimerRef) -> Int -> Effect Unit -> Effect TimerRef
foreign import cancel_ :: Pid -> Effect Unit

-- | Invokes the supplied effect every N milliseconds 
-- | See also timer:send_every in the OTP docs
-- | The process started by this will automatically terminate when the parent process dies
sendEvery :: forall state. Int -> Effect Unit -> Effect TimerRef
sendEvery = sendEvery_ TimerRef

-- | Invokes the supplied effect after N milliseconds 
-- | See also timer:send_every in the OTP docs
-- | The process started by this will automatically terminate when the parent process dies
sendAfter :: forall state. Int -> Effect Unit -> Effect TimerRef
sendAfter = sendAfter_ TimerRef

-- | Cancels a timer started by either sendEvery or sendAfter
-- | See also timer:cancel in the OTP docs
cancel :: TimerRef -> Effect Unit
cancel (TimerRef pid) = cancel_ pid
