module Pinto.ProcessT.Internal.Types
  ( class FFIParseT
  , class InitialState
  , class RunT
  , initialState
  , runT
  , psFromFFI
  )
  where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Erl.Process (ProcessM, unsafeRunProcessM)
import Foreign (Foreign)
import Type.Prelude (Proxy)
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




--------------------------------------------------------------------------------
-- All in one experiment...
--------------------------------------------------------------------------------
class ProcessT m mState appMsg | m -> mState appMsg where
  parseForeign :: Foreign -> m appMsg
  run :: forall a. m a -> mState -> Effect (Tuple a mState)
  initState :: Proxy m -> Effect mState

instance ProcessT (ProcessM appMsg) Unit appMsg where
  parseForeign = pure <<< unsafeCoerce
  run pm _ = do
    res <- unsafeRunProcessM pm
    pure $ Tuple res unit
  initState _ =  pure unit
