module Test.GenServer.ContStop
  ( genServer2Suite
  ) where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Process (Process, ProcessM, getProcess, self, send, toPid, (!))
import Erl.Process as Process
import Erl.Test.EUnit (TestF, suite, test)
import Foreign (unsafeToForeign)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.GenServer.ContStop (Action(..), InitResult(..), ContinueFn)
import Pinto.GenServer.ContStop as GS2
import Pinto.ProcessT (receive)
import Pinto.ProcessT.MonitorT (MonitorT, monitor, spawnLinkMonitor)
import Pinto.ProcessT.TrapExitT (TrapExitT)
import Pinto.Types (NotStartedReason(..), RegistryName(..), RegistryReference(..), StartLinkResult, crashIfNotStarted)
import Test.Assert (assert', assertEqual)
import Test.TestHelpers (getState, mpTest, setState, setStateCast, sleep)

type TestServerType
  = GS2.ServerType TestCont TestStop TestState (ProcessM TestMsg)

genServer2Suite :: Free TestF Unit
genServer2Suite =
  suite "Pinto genServer tests" do
    testStartLinkLocal
    testStartLinkGlobal
    testStopNormalLocal
    testStopNormalGlobal
    testMonadStatePassedAround

data TestState
  = TestState Int

derive instance eqTestState :: Eq TestState

instance showTestState :: Show TestState where
  show (TestState x) = "TestState: " <> show x


type TestState2 =
  { total :: Int
  , parentPid :: Process Int
  }

data TestCont
  = TestCont
  | TestContFrom (GS2.From TestState)

data TestMsg
  = TestMsg
  | TestMsgNotSent


data TestStop
  = StopReason

testStartLinkLocal :: Free TestF Unit
testStartLinkLocal =
  test "Can start a locally named GenServer" do
    testStartGetSet $ Local $ atom "testStartLinkLocal"


testStartLinkGlobal :: Free TestF Unit
testStartLinkGlobal =
  test "Can start a globally named GenServer" do
    testStartGetSet $ Global (unsafeToForeign $ atom "testStartLinkGlobal")

testStopNormalLocal :: Free TestF Unit
testStopNormalLocal =
  mpTest "Can start and stop a locally named GenServer" $
    testStopNormal $ Local $ atom "testStopNormalLocal"

testStopNormalGlobal :: Free TestF Unit
testStopNormalGlobal =
  mpTest "Can start and stop a globally named GenServer" $
    testStopNormal $ Global (unsafeToForeign $ atom "testStopNormalGlobal")



type TestMonad = MonitorT TestMonitorMsg (TrapExitT (ProcessM TestMsg))

testMonadStatePassedAround :: Free TestF Unit
testMonadStatePassedAround =
  mpTest "Ensure MonadProcessTrans state is maintained across calls" theTest
  where
  theTest :: ProcessM Int Unit
  theTest = do
    -- Go through each handler in a GenServer adding a monitor in each and check that they all fire
    -- The process we spawn waits 20ms before exit to make sure that all the monitors fire after the
    -- setup is complete and we don't just have messages in our process queue masking for the correct state
    -- Init (0x01) -> Continue (0x02) -> Info (0x04) -> Monitors and Exits fire -> terminate
    me <- self
    liftEffect $ void $ crashIfNotStarted <$> GS2.startLink' {init: init me, handleInfo, handleContinue, terminate}
    msg <- Process.receive
    liftEffect $ assertEqual
      { actual: msg
      , expected: 0x0F
      }

  init :: Process Int -> GS2.InitFn TestCont TestState2 TestMonad
  init parentPid = do
    _ <- spawnLinkMonitor exitsQuickly $ const (TestMonitorMsg 0x01)
    pure $ InitOkContinue {parentPid, total: 0} TestCont


  handleContinue :: ContinueFn TestCont TestStop TestState2 TestMonad
  handleContinue TestCont state = do
    -- let _ = spy "handleContinue" state
    me <- self
    void $ spawnLinkMonitor exitsQuickly $ const (TestMonitorMsg 0x02)
    liftEffect $ me ! TestMsg
    pure $ GS2.return state
  handleContinue (TestContFrom _) state = do
    pure $ GS2.return $ state

  handleInfo :: GS2.InfoFn TestCont TestStop _ TestState2  TestMonad
  handleInfo (Left msg) state = handleMonitorMsg msg state
  handleInfo (Right (Left msg)) state = handleExitMsg msg state
  handleInfo (Right (Right msg)) state = handleAppMsg msg state

  handleMonitorMsg (TestMonitorMsg i) state@{total: x} = do
    let
      -- _ =  spy "handleMonitorMsg" {i, state}
      newTotal = x + i
    case x of
      0 -> do
        void $ spawnLinkMonitor exitsQuickly $ const (TestMonitorMsg 0x08)
        pure $ GS2.return $ state{total =  newTotal}
      _ ->
        case newTotal of
          0x0F -> do
            pure $ GS2.returnWithAction StopNormal state{total =  newTotal}
          _ ->
            pure $ GS2.return $ state{total =  newTotal}

  handleExitMsg _msg state = do
    -- let
    --  _ =  spy "handleExitMsg" {_msg, state}
    pure $ GS2.return $ state

  handleAppMsg TestMsg state = do
    -- let
    --   _ =  spy "handleAppMsg" state

    void $ spawnLinkMonitor exitsQuickly $ const (TestMonitorMsg 0x04)
    pure $ GS2.return $ state
  handleAppMsg _ _state = do
    unsafeCrashWith "Unexpected message"

  terminate _reason {parentPid, total} = do
    liftEffect $ send parentPid total

  exitsQuickly :: ProcessM Void Unit
  exitsQuickly = do
    liftEffect $ sleep 20
    pure unit




type TrapExitState
  = { testPid :: Process Boolean
    , receivedExit :: Boolean
    , receivedTerminate :: Boolean
    }

testStartGetSet :: RegistryName TestServerType -> Effect Unit
testStartGetSet registryName = do
  let
    --gsSpec :: GS2.ServerSpec TestCont TestStop TestMsg _ TestState (ProcessM TestMsg)
    gsSpec :: GS2.GSConfig TestCont TestStop _ TestState (ProcessM TestMsg)
    gsSpec =
      (GS2.defaultSpec init)
        { serverName = Just registryName
        , handleInfo = Just handleInfo
        , handleContinue = Just handleContinue
        }

    instanceRef = ByName registryName
  serverPid <- crashIfNotStarted <$> (GS2.startLink gsSpec)
  maybeServerPid <- GS2.whereIs registryName
  assert' "The pid that is looked up should be that returned by start" $ maybeServerPid == Just serverPid
  getState instanceRef >>= expectState 0 -- Starts with initial state 0
  setState instanceRef (TestState 1) >>= expectState 0 -- Set new as 1, old is 0
  getState instanceRef >>= expectState 1 -- Previsouly set state returned
  setStateCast instanceRef (TestState 2) -- Set new state async
  getState instanceRef >>= expectState 2 -- Previsouly set state returned
  getProcess serverPid ! TestMsg -- Trigger HandleInfo to add 100
  getState instanceRef >>= expectState 102 -- Previsouly set state returned
  callContinueReply instanceRef >>= expectState 102 -- Trigger a continue - returning old state
  getState instanceRef >>= expectState 202 -- The continue fires updating state befor the next get
  callContinueNoReply instanceRef >>= expectState 202 -- Tigger a continue that replies in the continuation
  getState instanceRef >>= expectState 302 -- Post the reply, the continuation updates the state
  castContinue instanceRef -- Continues are triggered by casts as well
  getState instanceRef >>= expectState 402 -- As evidenced by the updated state
  stop instanceRef
  pure unit
  where
  init = do
    pure $ InitOk (TestState 0)

  handleInfo TestMsg (TestState x) = do
    pure $ GS2.return $ TestState $ x + 100

  handleInfo _ _s = do
    unsafeCrashWith "Unexpected message"

  handleContinue cont (TestState x) =
    case cont of
      TestCont -> pure $ GS2.return $ TestState $ x + 100
      TestContFrom from -> do
        GS2.replyTo from (TestState x)
        pure $ GS2.return $ TestState $ x + 100

  callContinueReply handle = GS2.call handle \_from state -> pure $ GS2.replyWithAction state (Continue TestCont) state

  callContinueNoReply handle = GS2.call handle \from state -> pure $ GS2.noReplyWithAction (Continue $ TestContFrom from) state

  castContinue handle = GS2.cast handle \state -> pure $ GS2.returnWithAction (Continue TestCont) state

  stop = GS2.stop

data TestMonitorMsg = TestMonitorMsg Int
derive instance Eq TestMonitorMsg
instance Show TestMonitorMsg where
  show (TestMonitorMsg i) = "TestMonitorMsg: " <> show i

testStopNormal :: RegistryName TestServerType -> MonitorT TestMonitorMsg (ProcessM Void) Unit
testStopNormal registryName = do
  let
    gsSpec :: GS2.GSConfig TestCont TestStop _ TestState (ProcessM TestMsg)
    gsSpec =
      (GS2.defaultSpec init)
        { serverName = Just registryName
        , handleInfo = Just handleInfo
        , handleContinue = Just handleContinue
        }
    instanceRef = ByName registryName
  serverPid <- liftEffect $ crashIfNotStarted <$> (GS2.startLink gsSpec)
  _ <- monitor (toPid $ getProcess serverPid) $ const (TestMonitorMsg 0)
  liftEffect do
    getState instanceRef >>= expectState 0 -- Starts with initial state 0
    -- Try to start the server again - should fail with already running
    (GS2.startLink gsSpec) <#> isAlreadyRunning >>= expect true
    triggerStopCast instanceRef
  msg <- receive
  liftEffect do
    expect msg (Left (TestMonitorMsg 0))
    void $ crashIfNotStarted <$> (GS2.startLink gsSpec)
    (GS2.startLink gsSpec) <#> isAlreadyRunning >>= expect true
    triggerStopCallReply instanceRef >>= expectState 42 -- New instance starts with initial state 0
    void $ crashIfNotStarted <$> (GS2.startLink gsSpec)
    getState instanceRef >>= expectState 0 -- New instance starts with initial state 0
    -- TODO trigger stop from a handle_info
    triggerStopCast instanceRef
  pure unit

  where
  init = do
    pure $ InitOk (TestState 0)

  handleInfo TestMsg (TestState x) = do
    pure $ GS2.return $ TestState $ x + 100

  handleInfo _ _s = do
    unsafeCrashWith "Unexpected message"

  handleContinue :: GS2.ContinueFn _ _ _ _
  handleContinue cont (TestState x) =
    case cont of
      TestCont -> pure $ GS2.return $ TestState $ x + 100
      TestContFrom from -> do
        GS2.replyTo from (TestState x)
        pure $ GS2.return $ TestState $ x + 100

  triggerStopCast handle = GS2.cast handle \state -> pure $ GS2.returnWithAction StopNormal state

  triggerStopCallReply handle = GS2.call handle \_from state -> pure $ GS2.replyWithAction (TestState 42) StopNormal state


---------------------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------------------
isAlreadyRunning :: forall serverType. StartLinkResult serverType -> Boolean
isAlreadyRunning = case _ of
  Left (AlreadyStarted _) -> true
  _ -> false

expectState :: Int -> TestState -> Effect Unit
expectState expected actual = assertEqual { actual, expected: TestState expected }

expect :: forall a. Eq a => Show a => a -> a -> Effect Unit
expect expected actual = assertEqual { actual, expected: expected }

