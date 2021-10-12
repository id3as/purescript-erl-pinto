-- | This module provides a means of using the timer functionality in core Erlang
-- | It'll work anywhere, it's up to you to route the messages sensibly once  you have  them in the callback
-- | Tip: See 'emitter' in Gen
module Pinto.Timer
  ( sendEvery
  , sendAfter
  , sendEveryTo
  , sendAfterTo
  , cancel
  , TimerRef
  ) where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Data.Time.Duration (Milliseconds)
import Erl.Process (class HasProcess, Process, getProcess, class HasSelf, self)

foreign import data TimerRef :: Type

-- | sends a message to 'self' every 'N' milliseconds
-- | See also timer:send_every in the OTP docs
-- | Note: This uses the old Timer API
sendEvery :: forall m msg.
  MonadEffect m =>
  HasSelf m msg =>
  Milliseconds ->
  msg -> m TimerRef
sendEvery t m = do
  self <- self
  liftEffect $ sendEveryToFFI t m self

-- | sends a message to 'self' after 'N' milliseconds
-- | See also erlang:send_after in the OTP docs
-- | Note: This uses the new Timer API
sendAfter :: forall m msg.
  MonadEffect m =>
  HasSelf m msg =>
  Milliseconds ->
  msg -> m TimerRef
sendAfter t m = do
  self <- self
  liftEffect $ sendAfterToFFI t m self

-- | Send `msg` to `process` every 'N' milliseconds
-- | See also timer:send_every in the OTP docs
-- | Note: This uses the old Timer API
sendEveryTo :: forall msg process. HasProcess msg process => Milliseconds -> msg -> process -> Effect TimerRef
sendEveryTo t m p = sendEveryToFFI t m $ getProcess p

-- | Send `msg` to `process` after 'N' milliseconds
-- | See also erlang:send_after in the OTP docs
-- | Note: This uses the new Timer API
sendAfterTo :: forall msg process. HasProcess msg process => Milliseconds -> msg -> process -> Effect TimerRef
sendAfterTo t m p = sendAfterToFFI t m $ getProcess p

-- | Given a TimerRef, cancels the timer as per timer:cancel/erlang:cancel_timer in the OTP docs
foreign import cancel :: TimerRef -> Effect Unit

foreign import sendEveryToFFI :: forall msg. Milliseconds -> msg -> Process msg -> Effect TimerRef
foreign import sendAfterToFFI :: forall msg. Milliseconds -> msg -> Process msg -> Effect TimerRef
