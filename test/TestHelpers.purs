module Test.TestHelpers
  ( mpTest
  )
  where

import Prelude

import Control.Monad.Free (Free)
import Effect.Class (class MonadEffect)
import Erl.Test.EUnit (TestF, test)
import Pinto.ProcessT (unsafeEvalProcess)
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans)

mpTest
  :: forall m mState appMsg parsedMsg
   . MonadProcessTrans m mState appMsg parsedMsg
  => MonadEffect m
  => String -> m Unit -> Free TestF Unit
mpTest desc mpt = test desc $ unsafeEvalProcess mpt
