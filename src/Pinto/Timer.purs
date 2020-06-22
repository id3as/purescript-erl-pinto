-- | This module provides a means of using the timer functionality in core Erlang
-- | It'll work anywhere, it's up to you to route the messages sensibly once  you have  them in the callback
-- | Tip: See 'emitter' in Gen

module Pinto.Timer 
  ( sendEvery,
    sendAfter,
    cancel,
    TimerRef ) 
    where

import Prelude

import Effect (Effect)
import Erl.Process.Raw (Pid)
import Pinto.MessageRouting as MR

type TimerRef = MR.RouterRef

foreign import sendEvery_ :: forall msg. Int -> msg -> Effect Pid
foreign import sendAfter_ :: forall msg. Int -> msg -> Effect Pid
foreign import cancel_ :: Pid -> Effect Unit

-- | invokes the callback every 'N' milliseconds
-- | See also timer:send_every in the OTP docs
sendEvery :: forall msg. Int -> msg -> (msg -> Effect Unit) -> Effect TimerRef
sendEvery x msg cb = do
  MR.startRouter (sendEvery_ x msg) cancel_ cb

-- | invokes the callback after 'N' milliseconds
-- | See also timer:send_after in the OTP docs
sendAfter :: forall msg.  Int -> msg -> (msg -> Effect Unit) -> Effect TimerRef
sendAfter x msg cb = do
  MR.startRouter (sendAfter_ x msg) cancel_ (\msg2 -> do 
                                                          _ <- MR.stopRouterFromCallback
                                                          _ <- cb msg2
                                                          pure unit)

-- | Cancels a timer started by either sendEvery or sendAfter
-- | See also timer:cancel in the OTP docs
cancel :: TimerRef -> Effect Unit
cancel = MR.stopRouter 
