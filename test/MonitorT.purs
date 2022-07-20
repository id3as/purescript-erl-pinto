module Test.MonitorT where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect.Class (liftEffect)
import Erl.Process (ProcessM, self, send, toPid, (!))
import Erl.Process as Process
import Erl.Process.Raw as Raw
import Erl.Test.EUnit (TestF, suite, test)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (evalProcess, receive, receiveWithTimeout, spawn)
import Pinto.ProcessT.Internal.Types (mySelf)
import Pinto.ProcessT.MonitorT (MonitorT, demonitor, monitor, spawnLinkMonitor, spawnMonitor)
import Test.Assert (assert, assertEqual)

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
    testProcM 

testProcM :: Free TestF Unit
testProcM = do
  test "unsafe send with evalProcess" do
    let
      theTest :: ProcessM Int Unit
      theTest = do
        liftEffect $ evalProcess inner
        msg <- receive
        let z = msg + 1
        liftEffect $ assertEqual { actual: z, expected: 42 }

      inner :: ProcessM String Unit
      inner = do
        me <- self
        liftEffect $ send me "hello"
        pure unit
    evalProcess theTest


  test "unsafe send with no self" do
    parent <- Raw.self
    let 
      proc1 :: ProcessM String Unit
      proc1 = do
        liftEffect $ evalProcess proc2

      proc2 :: ProcessM Int Unit
      proc2 = do
        n <- receive
        let res = n + 1
        liftEffect $ assertEqual { actual: res, expected: 42 }
        liftEffect $ Raw.send parent "done"


    p <- Process.spawnLink proc1
    send p "hello"
    
    _ <- Raw.receive -- ensuring the test doesn't exit before the spawned proc gets msg
    pure unit

testMonitor :: Free TestF Unit
testMonitor =
  test "Spawn a process and confirm we get a monitor message when it exits" do
    evalProcess theTest
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


testDemonitor :: Free TestF Unit
testDemonitor =
  test "Confirm we get no message on exit after a demonitor call" do
    evalProcess theTest
  where

  theTest :: MonitorT TestMonitorMsg (ProcessM TestAppMsg) Unit
  theTest = do
    pid <- liftEffect $ spawn immediatelyExitNormal
    -- This is bad! TODO
    -- me <- spy "me" $ liftEffect $ getTypedPid (Proxy :: Proxy (MonitorT TestMonitorMsg (ProcessM Int)))
    -- liftEffect $ me ! 7
    ref <- monitor pid $ const TestMonitorMsg
    demonitor ref
    meMySelf <- mySelf
    --liftEffect $ meMySelf ! TestAppMsg

    msg <- receiveWithTimeout (Milliseconds 2.0) TestTimeoutMsg
    case spy "msg" msg of
      Left TestTimeoutMsg -> pure unit
      Right _ ->
        unsafeCrashWith "We received a down after demonitor!"

testSpawnMonitor :: Free TestF Unit
testSpawnMonitor =
  test "spawnMonitor a process and confirm we get a monitor message when it exits" do
    evalProcess theTest
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
  test "spawnLinkMonitor a process, send it a message and confirm we get a monitor message when it exits" do
    evalProcess theTest
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
