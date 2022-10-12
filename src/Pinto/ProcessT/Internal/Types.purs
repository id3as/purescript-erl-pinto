module Pinto.ProcessT.Internal.Types
  ( ProcessM
  , ProcessTM
  , class MonadProcessHandled
  , class MonadProcessTrans
  , class MonadProcessRun
  , initialise
  , parseForeign
  , run
  , unsafeRunProcessTM
  )
  where

import Prelude

import Control.Monad.Identity.Trans (IdentityT(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (class HasReceive, class HasSelf, self)
import Erl.Process as Old
import Erl.Process.Raw as Raw
import Foreign (Foreign)
import Prim.TypeError as TE
import Type.Prelude (class TypeEquals, Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype ProcessTM :: Type -> Type -> Type -> Type
newtype ProcessTM userMsg handledMsg a = ProcessTM (Effect a)

-- For running ProcessTM without any transformers on top
type ProcessM msg = ProcessTM msg msg

derive newtype instance Functor (ProcessTM userMsg inMsg)
derive newtype instance Apply (ProcessTM userMsg inMsg)
derive newtype instance Applicative (ProcessTM userMsg inMsg)
derive newtype instance Bind (ProcessTM userMsg inMsg)
derive newtype instance Monad (ProcessTM userMsg inMsg)
derive newtype instance MonadEffect (ProcessTM userMsg inMsg)

unsafeRunProcessTM :: forall a b c. ProcessTM a b c -> Effect c
unsafeRunProcessTM (ProcessTM c) = c

instance HasSelf (ProcessTM userMsg handledMsg) userMsg where
  self = ProcessTM $ Old.unsafeRunProcessM self

-- Only works if ProcessTM is the top of the stack being run, i.e. there is no stack!
-- (or the stack just consists of IdentityT)
instance TypeEquals userMsg handledMsg => HasReceive (ProcessTM userMsg handledMsg) userMsg userMsg where
  receive = ProcessTM Raw.receive
  receiveWithTimeout t d = ProcessTM $ Raw.receiveWithTimeout t d

class MonadProcessTrans :: (Type -> Type) -> Type -> Type -> Type -> Constraint
class (MonadEffect m) <= MonadProcessTrans m mState appMsg outMsg | m -> mState appMsg outMsg where
  parseForeign :: Foreign -> m (Maybe outMsg)
class MonadProcessRun :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type -> Constraint
class (MonadEffect base, MonadProcessTrans m mState appMsg outMsg) <= MonadProcessRun base m mState appMsg outMsg | m -> mState appMsg outMsg where
  run :: forall a. m a -> mState -> base (Tuple a mState)
  initialise :: Proxy m -> base mState

instance MonadProcessTrans (ProcessTM appMsg handledMsg) Unit appMsg appMsg where
  parseForeign = pure <<< Just <<< unsafeCoerce
instance (MonadEffect base) => MonadProcessRun base (ProcessTM appMsg handledMsg) Unit appMsg appMsg where
  run pm _ = liftEffect do
    res <- unsafeRunProcessTM pm
    pure $ Tuple res unit
  initialise _ = pure unit

instance MonadProcessTrans m mState appMsg outMsg => MonadProcessTrans (IdentityT m) mState appMsg outMsg where
  parseForeign = IdentityT <<< parseForeign
instance MonadProcessRun base m mState appMsg outMsg => MonadProcessRun base (IdentityT m) mState appMsg outMsg where
  run (IdentityT m) = run m
  initialise _ = initialise (Proxy :: Proxy m)

class MonadProcessHandled :: (Type -> Type) -> Type -> Constraint
class MonadProcessHandled m handledMsg

instance (TypeEquals topMsg handledMsg) => MonadProcessHandled (ProcessTM appMsg handledMsg) topMsg
instance
  TE.Fail
    ( TE.Above
        (TE.Above (TE.Text "Usage of old type, please upgrade from") (TE.Beside (TE.Text "  ") (TE.Quote (Old.ProcessM appMsg))))
        (TE.Above (TE.Text "to the new type") (TE.Beside (TE.Text "  ") (TE.Quote (ProcessTM appMsg topMsg))))
    ) =>
  MonadProcessHandled (Old.ProcessM appMsg) topMsg
-- else instance MonadProcessHandled m handledMsg => MonadProcessHandled (stack m) handledMsg
