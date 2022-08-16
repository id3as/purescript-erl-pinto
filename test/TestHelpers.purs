module Test.TestHelpers
  ( getState
  , mpTest
  , setState
  , setStateCast
  , sleep
  )
  where

import Prelude

import Control.Monad.Free (Free)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Erl.Test.EUnit (TestF, test)
import Pinto.GenServer.ContStop as GS
import Pinto.ProcessT (unsafeEvalProcess)
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans)

mpTest
  :: forall m mState appMsg parsedMsg
   . MonadProcessTrans m mState appMsg parsedMsg
--  => MonadEffect m
  => String -> m Unit -> Free TestF Unit
mpTest desc mpt = test desc $ unsafeEvalProcess mpt

foreign import sleep :: Int -> Effect Unit


getState
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m
  => GS.ServerRef cont stop state m -> Effect state
getState handle =
  GS.call handle callFn
  where
  callFn :: GS.CallFn state cont stop state m
  callFn _from state =
    pure $ GS.reply state state


setState
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m
  => GS.ServerRef cont stop state m -> state -> Effect state
setState handle newState =
  GS.call handle callFn
  where
  callFn :: GS.CallFn state cont stop state m
  callFn _from oldState =
    pure $ GS.reply oldState newState

setStateCast
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m
  => GS.ServerRef cont stop state m -> state -> Effect Unit
setStateCast handle newState = GS.cast handle castFn
  where
    castFn :: GS.CastFn cont stop state m
    castFn _state = pure $ GS.return newState
