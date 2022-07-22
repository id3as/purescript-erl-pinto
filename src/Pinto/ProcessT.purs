module Pinto.ProcessT
  ( receive
  , receiveWithTimeout
  , spawn
  , spawnLink
  , unsafeEvalProcess
  , unsafeExecProcess
  , unsafeRunProcess
  , module TypesExport
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
import Pinto.ProcessT.Internal.Types (class HasSelf, self) as TypesExport
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

unsafeEvalProcess
  :: forall m mState appMsg parsedMsg a
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => m a -> Effect a
unsafeEvalProcess mpt =
  fst <$> unsafeRunProcess mpt

unsafeExecProcess
  :: forall m mState appMsg parsedMsg
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => m appMsg -> Effect mState
unsafeExecProcess mpt =
  snd <$> unsafeRunProcess mpt

unsafeRunProcess
  :: forall m mState appMsg parsedMsg a
   . MonadProcessTrans m mState appMsg parsedMsg
  => m a -> Effect (Tuple a mState)
unsafeRunProcess mpt =
  run mpt =<< initialise (Proxy :: Proxy m)

spawn
  :: forall m mState appMsg parsedMsg
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => m Unit -> Effect (Process appMsg)
spawn = unsafeCoerce <<< Raw.spawn <<< unsafeEvalProcess

spawnLink
  :: forall m mState appMsg parsedMsg
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => m Unit -> Effect (Process appMsg)
spawnLink = unsafeCoerce <<< Raw.spawnLink <<< unsafeEvalProcess


