module Pinto.ProcessT.Internal.Types
  ( class MonadProcessTrans
  , self
  , run
  , parseForeign
  , initialise
  , class HasSelf
  )
  where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (Process, ProcessM, unsafeRunProcessM)
import Erl.Process as Process
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

class HasSelf  (m :: Type -> Type) msg | m -> msg where
  self :: m (Process msg)

instance HasSelf (ProcessM a) a where
  self :: ProcessM a (Process a)
  self = Process.self

else instance
  ( MonadProcessTrans m mState appMsg outMsg
  , MonadEffect m
  ) =>
  HasSelf m appMsg where
    self = liftEffect $ unsafeRunProcessM Process.self
