module Pinto.ProcessT.Internal.Types
  ( class MonadProcessTrans
  , run
  , parseForeign
  , initialise
  )
  where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Process (ProcessM, unsafeRunProcessM)
import Foreign (Foreign)
import Type.Prelude (Proxy)
import Unsafe.Coerce (unsafeCoerce)

class MonadProcessTrans :: forall k. (Type -> Type) -> Type -> k -> Type -> Constraint
class MonadProcessTrans m mState appMsg outMsg | m -> mState appMsg outMsg where
  parseForeign :: Foreign -> m outMsg  -- appMsg TODO
  run :: forall a. m a -> mState -> Effect (Tuple a mState)
  initialise :: Proxy m -> Effect mState

instance MonadProcessTrans (ProcessM appMsg) Unit appMsg appMsg where
  parseForeign = pure <<< unsafeCoerce
  run pm _ = do
    res <- unsafeRunProcessM pm
    pure $ Tuple res unit
  initialise _ =  pure unit
