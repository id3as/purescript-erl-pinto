-- | Module roughly representing interactions with the 'application'
-- | See also 'application' in the OTP docs
module Pinto.App where

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn2, EffectFn2)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, tuple2)
import Foreign (Foreign)
import Pinto (StartLinkResult)
import Prelude ((<$>))
import Unsafe.Coerce (unsafeCoerce)

-- | Defines the entry point to an applicaiton that ignores any passed in arguments and simply calls the supervisor callback provided
-- |
-- | For example:
-- |
-- | ```purescript
-- | App.simpleStart MyGenSup.startLink
-- | ```
simpleStart :: forall args. Effect StartLinkResult -> EffectFn2 Atom (List args) (Tuple2 Atom Foreign)
simpleStart start = mkEffectFn2 (\_ _ -> eitherToOk <$> start)
  where
    eitherToOk :: forall a b c. Either a b -> Tuple2 Atom c
    eitherToOk (Left err) = tuple2 (atom "error") (unsafeCoerce err)
    eitherToOk (Right pid) = tuple2 (atom "ok") (unsafeCoerce pid)
