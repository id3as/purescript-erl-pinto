module Test.MonitorT where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Erl.Process (ProcessM, toPid, (!))
import Erl.Test.EUnit (TestF, suite, test)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (evalProcess, receive, spawn)
import Pinto.ProcessT.MonitorT (MonitorT, monitor, spawnLinkMonitor, spawnMonitor)

data TestMonitorMsg = TestMonitorMsg
data TestAppMsg = TestAppMsg

testMonitorT  :: Free TestF Unit
testMonitorT =
  suite "MonitorM tests" do
    testMonitor
    testSpawnMonitor
    testSpawnLinkMonitor

testMonitor :: Free TestF Unit
testMonitor =
  test "Spawn a process and confirm we get a monitor message when it exits" do
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

testSpawnMonitor :: Free TestF Unit
testSpawnMonitor =
  test "spawnMonitor a process and confirm we get a monitor message when it exits" do
    evalProcess theTest
  where

  theTest :: MonitorT TestMonitorMsg (ProcessM Void) Unit
  theTest= do
    _pid <- spawnMonitor immediatelyExitNormal $ const TestMonitorMsg
    msg <- receive
    case msg of
      Left TestMonitorMsg -> pure unit
      Right _ ->
        unsafeCrashWith "We got sent a void message!"


testSpawnLinkMonitor :: Free TestF Unit
testSpawnLinkMonitor =
  test "spawnLinkMonitor a process, send it a message and confirm we get a monitor message when it exits" do
    evalProcess theTest
  where

  theTest :: MonitorT TestMonitorMsg (ProcessM Void) Unit
  theTest= do
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
