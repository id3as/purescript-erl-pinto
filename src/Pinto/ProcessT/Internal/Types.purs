module Pinto.ProcessT.Internal.Types
  ( class MonadProcessTrans
--  , getTypedPid
  , mySelf
  , run
  , parseForeign
  , initialise
--  , class HasTypedPid
  , class MyHasSelf

  )
  where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect, class MonadEffect)
import Erl.Process (class HasSelf, Process, ProcessM, self, unsafeRunProcessM)
import Erl.Process.Raw as Raw
import Foreign (Foreign)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class MonadProcessTrans :: forall k. (Type -> Type) -> Type -> k -> Type -> Constraint
class MonadProcessTrans m mState appMsg outMsg | m -> mState appMsg outMsg where
  parseForeign :: Foreign -> m outMsg -- appMsg
  run :: forall a. m a -> mState -> Effect (Tuple a mState)
  initialise :: Proxy m -> Effect mState

instance MonadProcessTrans (ProcessM appMsg) Unit appMsg appMsg where
  parseForeign = pure <<< unsafeCoerce
  run pm _ = do
    res <- unsafeRunProcessM pm
    pure $ Tuple res unit
  initialise _ =  pure unit



class MyHasSelf  (m :: Type -> Type) msg | m -> msg where
  mySelf :: m (Process msg)


instance MyHasSelf (ProcessM a) a where
  mySelf :: forall a. ProcessM a (Process a)
  mySelf = unsafeCoerce 1

else instance
  ( MonadProcessTrans m mState appMsg outMsg
  , MonadEffect m
  ) =>
  MyHasSelf m appMsg where
    mySelf = liftEffect $ unsafeRunProcessM self
