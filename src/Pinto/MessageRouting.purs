module Pinto.MessageRouting ( startRouter
                            , maybeStartRouter
                            , stopRouterFromCallback
                            , stopRouter
                            , RouterRef(..)
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either)
import Effect (Effect)
import Erl.Process.Raw (Pid)

data RouterRef handle = RouterRef handle Pid

foreign import startRouterImpl :: forall handle msg. (handle -> Pid -> RouterRef handle) -> Effect handle -> (handle -> Effect Unit) -> (msg -> Effect Unit) -> Effect (RouterRef handle)
foreign import maybeStartRouterImpl :: forall handle msg. (handle -> Pid -> RouterRef handle) -> Effect (Maybe  handle) -> (handle -> Effect Unit) -> (msg -> Effect Unit) -> Effect (Maybe (RouterRef handle))
foreign import stopRouter  :: forall handle. RouterRef handle -> Effect Unit
foreign import stopRouterFromCallback :: Effect Unit

startRouter :: forall handle msg. Effect handle -> (handle -> Effect Unit) -> (msg -> Effect Unit) ->  Effect (RouterRef handle)
startRouter = startRouterImpl RouterRef

maybeStartRouter :: forall handle msg. Effect (Maybe handle) -> (handle -> Effect Unit) -> (msg -> Effect Unit) ->Effect (Maybe (RouterRef handle))
maybeStartRouter = maybeStartRouterImpl RouterRef
