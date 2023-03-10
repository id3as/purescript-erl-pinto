-- | This module is designed to wrap legacy APIs that send messages back to the
-- | invoking process, instead of receiving arbitrary types directly, this gives us a chance
-- | to intercept the legacy messages and lift them into an appropriate type for the current context
module Pinto.MessageRouting
  ( startRouter
  , maybeStartRouter
  , stopRouterFromCallback
  , stopRouter
  , RouterRef(..)
  ) where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Process.Raw (Pid)

-- | Reference to a running router
-- |
-- | - `handle` is the value returned by the start mechanism of the worker
data RouterRef handle = RouterRef handle Pid

-- | Given an `Effect handle`, runs that effect in a new process, returning a RouterRef for that new process
-- |
-- | The `(handle -> Effect Unit)` parameter will be invoked when the router is stopped
-- | and the `(msg -> Effect Unit)` parameter will be invoked whenever a message is received by the router
startRouter :: forall handle state msg. Effect handle -> (state -> handle -> Effect Unit) -> (state -> msg -> Effect state) -> state -> Effect (RouterRef handle)
startRouter = startRouterImpl RouterRef

-- | Given an `Effect (Maybe handle)`,  run that effect in a new process. If the effect returns Nothing, the process is terminated
-- | else, a `RouterRef handle` is returned
maybeStartRouter :: forall handle state msg. Effect (Maybe handle) -> (state -> handle -> Effect Unit) -> (state -> msg -> Effect state) -> state -> Effect (Maybe (RouterRef handle))
maybeStartRouter = maybeStartRouterImpl RouterRef

-- | Terminates a router by invoking the 'stop' mechanism registered when the router was first started with the
-- | `handle` that was returned by the initial Effect
foreign import stopRouter :: forall handle. RouterRef handle -> Effect Unit

-- | Instantly terminates a router from within a callback (such as `msg -> Effect Unit`) without access to the handle
-- |
-- | Note: This should only be called from within that callback as it results in a message being sent to the current process
foreign import stopRouterFromCallback :: Effect Unit

foreign import startRouterImpl :: forall handle state msg. (handle -> Pid -> RouterRef handle) -> Effect handle -> (state -> handle -> Effect Unit) -> (state -> msg -> Effect state) -> state -> Effect (RouterRef handle)

foreign import maybeStartRouterImpl :: forall handle state msg. (handle -> Pid -> RouterRef handle) -> Effect (Maybe handle) -> (state -> handle -> Effect Unit) -> (state -> msg -> Effect state) -> state -> Effect (Maybe (RouterRef handle))
