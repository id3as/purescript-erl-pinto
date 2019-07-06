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

foreign import sendEvery_ :: forall state. (Pid -> TimerRef) -> (ServerName state) -> Int -> (state -> Effect state) -> Effect TimerRef
foreign import sendAfter_ :: forall state. (Pid -> TimerRef) -> (ServerName state) -> Int -> (state -> Effect state) -> Effect TimerRef
foreign import cancel_ :: Pid -> Effect Unit

-- | Invokes the supplied callback within the context of the current gen server every
-- | N milliseconds (This API may be killed in the near future, as this is a silly way to do this)
-- | See also timer:send_every in the OTP docs
sendEvery :: forall state. ServerName state -> Int -> (state -> Effect state) -> Effect TimerRef
sendEvery = sendEvery_ TimerRef

-- | Invokes the supplied callback within the context of the current gen server after
-- | N milliseconds (This API may be killed in the near future, as this is a silly way to do this)
-- | See also timer:send_after in the OTP docs
sendAfter :: forall state. ServerName state -> Int -> (state -> Effect state) -> Effect TimerRef
sendAfter = sendAfter_ TimerRef

-- | Cancels a timer started by either sendEvery or sendAfter
-- | (This API may be killed in the near future, as this is a silly way to do this)
-- | See also timer:cancel in the OTP docs
cancel :: TimerRef -> Effect Unit
cancel (TimerRef pid) = cancel_ pid
