module Pinto.App where

import Effect (Effect)
import Effect.Uncurried (mkEffectFn2, EffectFn2)
import Erl.Atom (Atom)
import Erl.Data.List (List)
import Pinto (StartLinkResult)

simpleStart :: forall args. Effect StartLinkResult -> EffectFn2 Atom (List args) StartLinkResult
simpleStart start = mkEffectFn2 (\_ _ -> start)
