-- | Module roughly representing interactions with the 'application'
-- | See also 'application' in the OTP docs
module Pinto.App where

import Prelude

import Effect (Effect)
import Effect.Uncurried (mkEffectFn2, EffectFn2)
import Erl.Atom (Atom)
import Erl.Data.List (List)
import Foreign (Foreign)
import Pinto (StartLinkResult)
import Pinto.Sup (slrToForeign)

-- | Defines the entry point to an applicaiton that ignores any passed in arguments and simply calls the supervisor callback provided
-- |
-- | For example:
-- |
-- | ```purescript
-- | App.simpleStart MyGenSup.startLink
-- | ```
simpleStart :: forall args. Effect StartLinkResult -> EffectFn2 Atom (List args) Foreign
simpleStart start = mkEffectFn2 (\_ _ -> slrToForeign <$> start)
