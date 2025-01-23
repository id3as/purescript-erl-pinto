module Test.GenServer2
  ( genServer2Suite
  ) where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Erl.Process (Process, getProcess, self, send, (!), unsafeRunProcessM)
import Erl.Process as Legacy
import Erl.Test.EUnit (TestF, suite, test)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.GenServer2 (InitResult(..))
import Pinto.GenServer2 as GS2
import Erl.ProcessT (ProcessTM, ProcessM, receiveWithTimeout, spawnLink)
import Erl.ProcessT.MonitorT (MonitorT)
import Erl.ProcessT.TrapExitT (TrapExitT, ExitMessage(..))
import Pinto.Types (RegistryReference(..), crashIfNotStarted)
import Test.Assert (assertEqual, assertEqual')
import Test.TestHelpers (getState, mpTest, setState, setStateCast)

genServer2Suite :: Free TestF Unit
genServer2Suite =
  suite "Pinto genServer tests" do
    testStartLinkAnonymous
    testHandleInfo
    testCall
    testCast
    testTrapExits

data TestState = TestState Int

derive instance eqTestState :: Eq TestState

instance showTestState :: Show TestState where
  show (TestState x) = "TestState: " <> show x

type TestState2 =
  { total :: Int
  , parentPid :: Process Int
  }

data TestMsg
  = TestMsg
  | TestMsgNotSent

testStartLinkAnonymous :: Free TestF Unit
testStartLinkAnonymous =
  test "Can start an anonymous GenServer" do
    serverPid <- crashIfNotStarted <$> GS2.startLink' { init }
    let
      instanceRef = ByPid serverPid
    state1 <- getState instanceRef
    state2 <- setState instanceRef (TestState 1)
    state3 <- getState instanceRef
    setStateCast instanceRef (TestState 2)
    state4 <- getState instanceRef
    assertEqual { actual: state1, expected: TestState 0 }
    assertEqual { actual: state2, expected: TestState 0 }
    assertEqual { actual: state3, expected: TestState 1 }
    assertEqual { actual: state4, expected: TestState 2 }
    pure unit
  where
  init :: GS2.InitFn TestState (ProcessTM Void Void)
  init = pure $ GS2.InitOk (TestState 0)

testHandleInfo :: Free TestF Unit
testHandleInfo =
  test "HandleInfo handler receives message" do
    serverPid <- crashIfNotStarted <$> GS2.startLink' { init, handleInfo }
    getProcess serverPid ! TestMsg
    state <- getState (ByPid serverPid)
    assertEqual
      { actual: state
      , expected: TestState 1
      }
    pure unit
  where
  init :: GS2.InitFn TestState (ProcessTM TestMsg TestMsg)
  init = do
    pure $ GS2.InitOk $ TestState 0

  handleInfo TestMsg (TestState x) = do
    pure $ GS2.return $ TestState $ x + 1

  handleInfo _ _s = do
    unsafeCrashWith "Unexpected message"

type TestMonad = MonitorT TestMonitorMsg (TrapExitT (ProcessTM TestMsg TestMsg))

testCall :: Free TestF Unit
testCall =
  test "Can create gen_server:call handlers" do
    serverPid <- crashIfNotStarted <$> (GS2.startLink $ GS2.defaultSpec init)
    state <- getState (ByPid serverPid)
    assertEqual
      { actual: state
      , expected: TestState 7
      }
    pure unit
  where
  init :: GS2.InitFn TestState (ProcessM Void)
  init = do
    pure $ InitOk $ TestState 7

testCast :: Free TestF Unit
testCast =
  test "HandleCast changes state" do
    serverPid <- crashIfNotStarted <$> (GS2.startLink $ (GS2.defaultSpec init))
    setStateCast (ByPid serverPid) $ TestState 42
    state <- getState (ByPid serverPid)
    assertEqual
      { actual: state
      , expected: TestState 42
      }
    pure unit
  where
  init :: GS2.InitFn TestState (ProcessM Void)
  init = do
    pure $ InitOk $ TestState 0

type TrapExitState =
  { testPid :: Process Boolean
  , receivedExit :: Boolean
  , receivedTerminate :: Boolean
  }

testTrapExits :: Free TestF Unit
testTrapExits =
  suite "Trapped exits" do
    mpTest "Children's exits get translated when requested" testChildExit
    mpTest "Parent exits arrive in the terminate callback" testParentExit

  where
  testChildExit :: ProcessM Void Unit
  testChildExit = liftEffect do
    serverPid <- crashIfNotStarted <$> (GS2.startLink' { init, handleInfo })

    let waitForValidState  = do 
          state <- getState (ByPid serverPid)
          if state.receivedExit then pure state
          else do 
            unsafeRunProcessM do
              _ <- Legacy.receiveWithTimeout (Milliseconds 50.0) unit
              pure unit
            waitForValidState
   
    state <- waitForValidState  

    assertEqual
      { actual: state.receivedExit
      , expected: true
      }
    pure unit

  testParentExit :: ProcessM Boolean Unit
  testParentExit = do
    testPid <- self
    let
      spawnAndExit :: ProcessM Void Unit
      spawnAndExit = void $ liftEffect $ crashIfNotStarted <$> (GS2.startLink' $ { init: init2 testPid, terminate })

    void $ liftEffect $ spawnLink spawnAndExit
    actual <- receiveWithTimeout (Milliseconds 50.0)
    liftEffect $ assertEqual' "Terminate wasn't called on the genserver" { expected: Right true, actual }

  init :: GS2.InitFn _ (TrapExitT (ProcessTM Void _))
  init = do
    pid <- liftEffect $ spawnLink exitsImmediately
    pure $ InitOk $ { testPid: pid, receivedExit: false, receivedTerminate: false }

  init2 :: Process Boolean -> GS2.InitFn TrapExitState (TrapExitT (ProcessTM Void _))
  init2 pid = do
    pure $ InitOk $ { testPid: pid, receivedExit: false, receivedTerminate: false }

  handleInfo (Left (Exit _ _)) s = do
    pure $ GS2.return $ s { receivedExit = true }

  handleInfo _ _s = do
    unsafeCrashWith "Unexpected message"

  terminate _reason s = do
    liftEffect $ send s.testPid true

data TestMonitorMsg = TestMonitorMsg Int

derive instance Eq TestMonitorMsg
instance Show TestMonitorMsg where
  show (TestMonitorMsg i) = "TestMonitorMsg: " <> show i

---------------------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------------------
exitsImmediately :: forall handledMsg. ProcessTM Void handledMsg Unit
exitsImmediately = do 
  pure unit
