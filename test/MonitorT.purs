module Test.MonitorT where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Tuple (Tuple, fst)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Process (Process, ProcessM, toPid)
import Erl.Process.Raw as Raw
import Erl.Test.EUnit (TestF, runTests, suite, test)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (receive, runProcessT, spawn)
import Pinto.ProcessT.MonitorT (MonitorT, monitor)
import Unsafe.Coerce (unsafeCoerce)

data TestMonitorMsg = TestMonitorMsg
data TestAppMsg = TestAppMsg

testMonitorT  :: Free TestF Unit
testMonitorT =
  suite "MonitorM tests" do
    testMonitor

testMonitor  :: Free TestF Unit
testMonitor =
  test "Start a process and confirm we get a monitor message when it exits" do
    runProcessT theTest
  where

  theTest :: MonitorT TestMonitorMsg (ProcessM Void) Unit
  theTest= do
    pid <- liftEffect $ spawn immediatelyExitNormal
    _ <- monitor (toPid pid) $ const TestMonitorMsg
    msg <- receive
    case msg of
      Left TestMonitorMsg -> pure unit
      Right _ ->
        unsafeCrashWith "We got sent a void message!"



bar :: ProcessM Void Int
bar = pure 1


immediatelyExitNormal :: ProcessM Void Unit
immediatelyExitNormal = pure unit



-- runFoo :: MonitorT TestMonitorMsg (ProcessM Void) Unit -> Effect Unit
-- runFoo = runProcessT




      --void $
