module Test.MonitorT where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Erl.Process (ProcessM, toPid)
import Erl.Test.EUnit (TestF, suite, test)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (evalProcess, receive, spawn)
import Pinto.ProcessT.MonitorT (MonitorT, monitor)

data TestMonitorMsg = TestMonitorMsg
data TestAppMsg = TestAppMsg

testMonitorT  :: Free TestF Unit
testMonitorT =
  suite "MonitorM tests" do
    testMonitor

testMonitor  :: Free TestF Unit
testMonitor =
  test "Start a process and confirm we get a monitor message when it exits" do
    evalProcess theTest
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

immediatelyExitNormal :: ProcessM Void Unit
immediatelyExitNormal = pure unit
