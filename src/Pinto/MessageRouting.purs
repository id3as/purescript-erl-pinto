module Pinto.MessageRouting where

import Prelude
import Effect (Effect)
import Erl.Process.Raw (Pid)

newtype RouterRef = RouterRef Pid

foreign import startRouter :: forall handle msg. Effect handle -> (handle -> Effect Unit) -> (msg -> Effect Unit) -> Effect RouterRef
foreign import stopRouter :: RouterRef -> Effect Unit
foreign import stopRouterFromCallback :: Effect Unit
