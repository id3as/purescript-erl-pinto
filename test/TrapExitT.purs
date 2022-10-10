module Test.TrapExitT where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Erl.Test.EUnit (TestF, suite)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (ProcessTM, ProcessM, receive, spawnLink)
import Pinto.ProcessT.TrapExitT (TrapExitT)
import Test.TestHelpers (mpTest)

data TestAppMsg = TestAppMsg
data TestTimeoutMsg = TestTimeoutMsg

testTrapExitT :: Free TestF Unit
testTrapExitT =
  suite "TrapExitM tests" do
    testTrapExit

testTrapExit :: Free TestF Unit
testTrapExit =
  mpTest "Spawn a process and confirm we get a message when it exits" theTest
  where

  theTest :: TrapExitT (ProcessTM TestAppMsg _) Unit
  theTest = do
    _pid <- liftEffect $ spawnLink immediatelyExitNormal
    msg <- receive
    case msg of
      Left _exitMSg -> pure unit
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

immediatelyExitNormal :: ProcessM Void Unit
immediatelyExitNormal = pure unit
