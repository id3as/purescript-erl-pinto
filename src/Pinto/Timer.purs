-- | This module provides a means of using the timer functionality in core Erlang
-- | It'll work anywhere, it's up to you to route the messages sensibly once  you have  them in the callback
-- | Tip: See 'emitter' in Gen

module Pinto.Timer 
  ( sendEvery,
    sendAfter,
    cancel,
    TimerRef
  ) 
    where

import Prelude

import Effect (Effect)
import Erl.Process (Process)

foreign import data TimerRef :: Type

-- | invokes the callback every 'N' milliseconds
-- | See also timer:send_every in the OTP docs
-- | Note: This uses the old Timer API
foreign import sendEvery :: forall msg. Int -> msg -> Process msg -> Effect TimerRef

-- | invokes the callback after 'N' milliseconds
-- | See also erlang:send_after in the OTP docs
-- | Note: This uses the new Timer API
foreign import sendAfter :: forall msg.  Int -> msg -> Process msg -> Effect TimerRef

-- | Given a TimerRef, cancels the timer as per timer:cancel/erlang:cancel_timer in the OTP docs
foreign import cancel :: TimerRef -> Effect Unit
