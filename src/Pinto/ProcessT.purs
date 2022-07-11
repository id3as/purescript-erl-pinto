module Pinto.ProcessT
  ( class FFIParseT
  , class InitialState
  , class RunT
  , initialState
  , receive
  , runT
  , psFromFFI
  )
  where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (ProcessM, unsafeRunProcessM)
import Erl.Process.Raw as Raw
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)


class InitialState state where
  initialState :: Effect state

instance InitialState Unit where
  initialState = pure unit

instance (InitialState a, InitialState b) => InitialState (Tuple a b) where
  initialState = do
    a <- initialState
    b <- initialState
    pure $ Tuple a b


class FFIParseT :: forall k. (k -> Type) -> k -> Constraint
class FFIParseT m msg | m -> msg where
  psFromFFI :: Foreign -> m msg

instance FFIParseT (ProcessM outMsg) outMsg where
  psFromFFI   = pure <<< unsafeCoerce


class RunT m state | m -> state where
  runT
    :: forall a
     . m a -> state -> Effect (Tuple a state)

instance RunT (ProcessM outMsg) Unit where
  runT t _ = do
    res <-  unsafeRunProcessM t
    pure $ Tuple res unit


receive
  :: forall m outMsg.
     FFIParseT m outMsg =>
     MonadEffect m =>
     m outMsg
receive = do
  msg <- liftEffect Raw.receive
  psFromFFI msg


--------------------------------------------------------------------------------
-- All in one experiment...
--------------------------------------------------------------------------------
class ProcessT m mState appMsg | m -> mState appMsg where
  fromFFI :: Foreign -> m appMsg
  runT2 :: forall a. m a -> mState -> Effect (Tuple a mState)
  initState :: m mState

instance ProcessT (ProcessM appMsg) Unit appMsg where
  fromFFI = pure <<< unsafeCoerce
  runT2 pm _ = do
    res <- unsafeRunProcessM pm
    pure $ Tuple res unit
  initState =  pure unit
