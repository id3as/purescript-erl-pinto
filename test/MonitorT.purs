module Test.MonitorT where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Erl.Process (ProcessM, toPid, (!))
import Erl.Test.EUnit (TestF, suite)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (Timeout(..), receive, receiveWithTimeout, spawn, spawnLink)
import Pinto.ProcessT.MonitorT (MonitorT, demonitor, monitor, spawnLinkMonitor, spawnMonitor)
import Pinto.ProcessT.TrapExitT (TrapExitT)
import Pinto.Types (ExitMessage(..))
import Test.Assert (assertEqual)
import Test.TestHelpers (mpTest)

data TestMonitorMsg = TestMonitorMsg
data TestAppMsg = TestAppMsg
data TestTimeoutMsg = TestTimeoutMsg

testMonitorT  :: Free TestF Unit
testMonitorT =
  suite "MonitorM tests" do
    testMonitor
    testDemonitor
    testSpawnMonitor
    testSpawnLinkMonitor
    testMonitorTrapExit
    testTrapExitMonitor

testMonitor :: Free TestF Unit
testMonitor =
  mpTest "Spawn a process and confirm we get a monitor message when it exits" theTest
  where

  theTest :: MonitorT TestMonitorMsg (ProcessM Void) Unit
  theTest = do
    pid <- liftEffect $ spawn immediatelyExitNormal
    _ <- monitor (toPid pid) $ const TestMonitorMsg
    msg <- receive
    case msg of
      Left TestMonitorMsg -> pure unit
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

testMonitorTrapExit :: Free TestF Unit
testMonitorTrapExit =
  mpTest "Spawn a process and confirm we get both a monitor and exit message" theTest
  where

  theTest :: TrapExitT (MonitorT TestMonitorMsg (ProcessM Void)) Unit
  theTest = do
    pid <- liftEffect $ spawnLink immediatelyExitNormal
    _ <- lift $ monitor (toPid pid) $ const TestMonitorMsg
    first <- handleMsg
    second <- handleMsg
    liftEffect $ assertEqual { actual: first + second, expected: 3 }

  handleMsg = do
    msg <- receive
    case msg of
      Left (Exit _ _) -> pure 1
      Right (Left TestMonitorMsg) -> pure 2
      Right (Right _) ->
        unsafeCrashWith "We got sent a void message!"


testTrapExitMonitor :: Free TestF Unit
testTrapExitMonitor =
  mpTest "Spawn a process and confirm we get both a monitor and exit message" theTest
  where

  theTest :: MonitorT TestMonitorMsg (TrapExitT (ProcessM Void)) Unit
  theTest = do
    pid <- liftEffect $ spawnLink immediatelyExitNormal
    _ <- monitor (toPid pid) $ const TestMonitorMsg
    first <- handleMsg
    second <- handleMsg
    liftEffect $ assertEqual { actual: first + second, expected: 3 }

  handleMsg = do
    msg <- receive
    case msg of
      Left TestMonitorMsg -> pure 1
      Right (Left (Exit _ _)) -> pure 2
      Right (Right _) ->
        unsafeCrashWith "We got sent a void message!"

testDemonitor :: Free TestF Unit
testDemonitor =
  mpTest "Confirm we get no message on exit after a demonitor call" theTest
  where

  theTest :: MonitorT TestMonitorMsg (ProcessM TestAppMsg) Unit
  theTest = do
    pid <- liftEffect $ spawn immediatelyExitNormal
    ref <- monitor pid $ const TestMonitorMsg
    demonitor ref

    msg <- receiveWithTimeout (Milliseconds 2.0)
    case msg of
      Left Timeout -> pure unit
      Right _ ->
        unsafeCrashWith "We received a down after demonitor!"

testSpawnMonitor :: Free TestF Unit
testSpawnMonitor =
  mpTest "spawnMonitor a process and confirm we get a monitor message when it exits" theTest
  where

  theTest :: MonitorT TestMonitorMsg (ProcessM Void) Unit
  theTest = do
    _pid <- spawnMonitor immediatelyExitNormal $ const TestMonitorMsg
    msg <- receive
    case msg of
      Left TestMonitorMsg -> pure unit
      Right _ ->
        unsafeCrashWith "We got sent a void message!"


testSpawnLinkMonitor :: Free TestF Unit
testSpawnLinkMonitor =
  mpTest "spawnLinkMonitor a process, send it a message and confirm we get a monitor message when it exits" theTest
  where

  theTest :: MonitorT TestMonitorMsg (ProcessM Void) Unit
  theTest = do
    pid <- spawnLinkMonitor exitOnMessage $ const TestMonitorMsg
    liftEffect $ pid ! TestAppMsg
    msg <- receive
    case msg of
      Left TestMonitorMsg -> pure unit
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

immediatelyExitNormal :: ProcessM Void Unit
immediatelyExitNormal = pure unit


exitOnMessage :: ProcessM TestAppMsg Unit
exitOnMessage = do
  msg <- receive
  case msg of
    TestAppMsg -> pure unit
