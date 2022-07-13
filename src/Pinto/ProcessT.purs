module Pinto.ProcessT
  ( evalProcess
  , execProcess
  , receive
  , runProcess
  , spawn
  , spawnLink
  )
  where

import Prelude

import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (Process)
import Erl.Process.Raw as Raw
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans, initialise, parseForeign, run)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

receive
  :: forall m mState appMsg outMsg
   . MonadProcessTrans m mState appMsg outMsg
  => MonadEffect m
  => m outMsg
receive = parseForeign =<< liftEffect Raw.receive


-- run / eval / exec to mirror the functions from StateT etc
-- names?
--   evalMonadProcessTransformer
--   evalMonadProcess
--   evalProcess
--   evalMP / MPT / ...
--   eval

evalProcess
  :: forall m mState appMsg outMsg a
   . MonadProcessTrans m mState appMsg outMsg
  => MonadEffect m
  => m a -> Effect a
evalProcess mpt =
  fst <$> runProcess mpt

execProcess
  :: forall m mState appMsg outMsg
   . MonadProcessTrans m mState appMsg outMsg
  => MonadEffect m
  => m appMsg -> Effect mState
execProcess mpt =
  snd <$> runProcess mpt

runProcess
  :: forall m mState appMsg outMsg a
   . MonadProcessTrans m mState appMsg outMsg
  => m a -> Effect (Tuple a mState)
runProcess mpt =
  run mpt =<< initialise (Proxy :: Proxy m)

spawn
  :: forall m mState appMsg outMsg
   . MonadProcessTrans m mState appMsg outMsg
  => MonadEffect m
  => m Unit -> Effect (Process appMsg)
spawn = unsafeCoerce <<< Raw.spawn <<< evalProcess

spawnLink
  :: forall m mState appMsg outMsg
   . MonadProcessTrans m mState appMsg outMsg
  => MonadEffect m
  => m Unit -> Effect (Process appMsg)
spawnLink = unsafeCoerce <<< Raw.spawnLink <<< evalProcess
