module Pinto.ProcessT
  ( evalProcess
  , execProcess
  , receive
  , receiveWithTimeout
  , runProcess
  , spawn
  , spawnLink
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (Process)
import Erl.Process.Raw as Raw
import Foreign (unsafeToForeign)
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans, initialise, parseForeign, run)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

receive
  :: forall m mState appMsg parsedMsg
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => m parsedMsg
receive = parseForeign =<< liftEffect Raw.receive


data PrivateProcessTTimeoutMsg
  = PrivateProcessTTimeoutMsg__
  | ThereToGetRidOfUnreacableWarning

receiveWithTimeout
  :: forall m mState appMsg parsedMsg timeoutMsg
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => Milliseconds -> timeoutMsg -> m (Either timeoutMsg parsedMsg)
receiveWithTimeout ms timeoutMsg = do
  rawMsg  <- liftEffect $ Raw.receiveWithTimeout ms PrivateProcessTTimeoutMsg__
  case rawMsg of
    PrivateProcessTTimeoutMsg__ ->
      pure $ Left timeoutMsg
    _ -> do
      parsed <- parseForeign $ unsafeToForeign rawMsg
      pure $ Right parsed

-- run / eval / exec to mirror the functions from StateT etc
-- names?
--   evalMonadProcessTransformer
--   evalMonadProcess
--   evalProcess
--   evalMP / MPT / ...
--   eval

evalProcess
  :: forall m mState appMsg parsedMsg a
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => m a -> Effect a
evalProcess mpt =
  fst <$> runProcess mpt

execProcess
  :: forall m mState appMsg parsedMsg
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => m appMsg -> Effect mState
execProcess mpt =
  snd <$> runProcess mpt

runProcess
  :: forall m mState appMsg parsedMsg a
   . MonadProcessTrans m mState appMsg parsedMsg
  => m a -> Effect (Tuple a mState)
runProcess mpt =
  run mpt =<< initialise (Proxy :: Proxy m)

spawn
  :: forall m mState appMsg parsedMsg
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => m Unit -> Effect (Process appMsg)
spawn = unsafeCoerce <<< Raw.spawn <<< evalProcess

spawnLink
  :: forall m mState appMsg parsedMsg
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => m Unit -> Effect (Process appMsg)
spawnLink = unsafeCoerce <<< Raw.spawnLink <<< evalProcess
