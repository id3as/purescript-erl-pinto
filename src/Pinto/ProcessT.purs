module Pinto.ProcessT
  ( receive
  , runProcessT
  , spawn
  )
  where

import Prelude

import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (Process)
import Erl.Process.Raw as Raw
import Pinto.ProcessT.Internal.Types (class FFIParseT, class InitialState, class RunT, initialState, psFromFFI, runT)
import Unsafe.Coerce (unsafeCoerce)


receive
  :: forall m outMsg.
     FFIParseT m outMsg =>
     MonadEffect m =>
     m outMsg
receive = do
  msg <- liftEffect Raw.receive
  psFromFFI msg


runProcessT
  :: forall a state m
   . RunT m state
   => InitialState state
   => m a -> Effect a
runProcessT processT = do
  is <- initialState
  fst <$> runT processT is


spawn :: forall m state msg
         . RunT m state
        => InitialState state
        => FFIParseT m msg
        => m Unit -> Effect (Process msg)
spawn = unsafeCoerce <<< Raw.spawn <<< runProcessT
