module Test.GenServer2
  ( genServer2Suite
  ) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Process (Process, ProcessM, getProcess, (!))
import Erl.Process.Raw (Pid)
import Erl.Process.Raw as Raw
import Erl.Test.EUnit (TestF, suite, test)
import Foreign (unsafeToForeign)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.GenServer2 (InitResult(..))
import Pinto.GenServer2 as GS2
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans)
import Pinto.ProcessT.MonitorT (MonitorT, spawnMonitor)
import Pinto.Types (ExitMessage, NotStartedReason(..), RegistryName(..), RegistryReference(..), StartLinkResult, crashIfNotStarted)
import Test.Assert (assert', assertEqual)
import Test.ValueServer as ValueServer
import Unsafe.Coerce (unsafeCoerce)

foreign import sleep :: Int -> Effect Unit

type TestServerType
  = GS2.ServerType TestCont TestStop TestMsg TestState

genServer2Suite :: Free TestF Unit
genServer2Suite =
  suite "Pinto genServer tests" do
    testStartLinkAnonymous
    -- testStartLinkLocal
    -- testStartLinkGlobal
    -- testStopNormalLocal
    -- testHandleInfo
    -- testCall
    -- testCast
    -- testValueServer
    -- --testTrapExits
    -- testMonadStatePassedAround

data TestState
  = TestState Int

derive instance eqTestState :: Eq TestState

instance showTestState :: Show TestState where
  show (TestState x) = "TestState: " <> show x

data TestCont
  = TestCont
  | TestContFrom (GS2.From TestState)

data TestMsg
  = TestMsg
  | TrappedExit ExitMessage

data TestStop
  = StopReason

data MonitorMsg = MonitorMsg

testStartLinkAnonymous :: Free TestF Unit
testStartLinkAnonymous =
  test "Can start an anonymous GenServer" do
    let
      gsConfig = GS2.defaultSpec init

      x = GS2.startLink3 gsConfig
    serverPid :: ?t <- crashIfNotStarted <$> x
    let
      instanceRef = ByPid serverPid
    -- state1 <- getState instanceRef
    -- state2 <- setState instanceRef (TestState 1)
    -- state3 <- getState instanceRef
    -- setStateCast instanceRef (TestState 2)
    -- state4 <- getState instanceRef
    -- assertEqual { actual: state1, expected: TestState 0 }
    -- assertEqual { actual: state2, expected: TestState 0 }
    -- assertEqual { actual: state3, expected: TestState 1 }
    -- assertEqual { actual: state4, expected: TestState 2 }
    pure unit
  where
  init :: GS2.InitFn Void TestState (ProcessM Void)
  init = do
    let x :: ?t
        x = GS2.InitOk (TestState 0)
    unsafeCoerce 1

{-
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
  test "Can start and stop a locally named GenServer" do
    testStopNormal $ Local $ atom "testStopNormalLocal"

testStopNormalGlobal :: Free TestF Unit
testStopNormalGlobal =
  test "Can start and stop a globally named GenServer" do
    testStopNormal $ Global (unsafeToForeign $ atom "testStopNormalGlobal")

testHandleInfo :: Free TestF Unit
testHandleInfo =
  test "HandleInfo handler receives message" do
    serverPid <- crashIfNotStarted <$> (GS2.startLink $ (GS2.defaultSpec init) { handleInfo = Just handleInfo })
    (unsafeCoerce serverPid :: Process TestMsg) ! TestMsg
    state <- getState (ByPid serverPid)
    assertEqual
      { actual: state
      , expected: TestState 1
      }
    pure unit
  where
  init :: GS2.InitFn _ _  _ _ _ (ProcessM TestMsg)
  init = do
    pure $ GS2.InitOk $ TestState 0

  handleInfo TestMsg (TestState x) = do
    pure $ GS2.return $ TestState $ x + 1

  handleInfo _ _s = do
    unsafeCrashWith "Unexpected message"

testMonadStatePassedAround :: Free TestF Unit
testMonadStatePassedAround =
  test "Ensure MonadProcessTrans state is maintained across calls" do
    serverPid <- crashIfNotStarted <$> (GS2.startLink $ (GS2.defaultSpec init) { handleInfo = Just handleInfo })
    getProcess serverPid ! TestMsg
    sleep 10
    state <- getState (ByPid serverPid)
    assertEqual
      { actual: state
      , expected: TestState 21
      }
    pure unit
  where
  init :: InitFn TestCont TestStop TestMsg (Either MonitorMsg TestMsg) TestState (MonitorT MonitorMsg (ProcessM TestMsg))
  init = do
    _ <- lift $ spawnMonitor exitsImmediately $ const MonitorMsg
    pure $ InitOk $ TestState 0

  --handleInfo :: InfoFn2 TestCont TestStop TestMsg TestState (MonitorT MonitorMsg (ProcessM TestMsg))
  --handleInfo :: ?t
  handleInfo (Right TestMsg) (TestState x) = do
    _ <- lift $ spawnMonitor exitsImmediately $ const MonitorMsg
    pure $ GS2.return $ TestState $ x + 1

  handleInfo (Left MonitorMsg) (TestState x) = do
    pure $ GS2.return $ TestState $ x + 10

  handleInfo _ _s = do
    unsafeCrashWith "Unexpected message"

exitsImmediately :: ProcessM Void Unit
exitsImmediately = pure unit

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
  init :: InitFn _ _ _  _ _ (ProcessM Void)
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
  init :: InitFn _ _  _ _ _ (ProcessM Void)
  init = do
    pure $ InitOk $ TestState 0

testValueServer :: Free TestF Unit
testValueServer =
  test "Interaction with gen_server with closed API" do
    void $ ValueServer.startLink
    void $ ValueServer.setValue 42
    v1 <- ValueServer.setValue 43
    v2 <- ValueServer.getValue
    ValueServer.setValueAsync 50
    v3 <- ValueServer.getValue
    assertEqual { actual: v1, expected: 42 }
    assertEqual { actual: v2, expected: 43 }
    assertEqual { actual: v3, expected: 50 }
    pure unit

type TrapExitState
  = { testPid :: Pid
    , receivedExit :: Boolean
    , receivedTerminate :: Boolean
    }
-}
{-testTrapExits :: Free TestF Unit
testTrapExits =
  suite "Trapped exits" do
    test "Children's exits get translated when requested" do
      serverPid <- crashIfNotStarted <$> (GS2.startLink $ (GS2.defaultSpec init) { handleInfo = Just handleInfo, trapExits = Just TrappedExit })
      state <- getState (ByPid serverPid)
      assertEqual
        { actual: state.receivedExit
        , expected: true
        }
      pure unit
    test "Parent exits arrive in the terminate callback" do
      testPid <- Raw.self
      void $ Raw.spawnLink $ void $ crashIfNotStarted <$> (GS2.startLink $ (GS2.defaultSpec $ init2 testPid) { terminate = Just terminate, trapExits = Just TrappedExit })
      receivedTerminate <- Raw.receiveWithTimeout (Milliseconds 500.0) false
      assert' "Terminate wasn't called on the genserver" receivedTerminate
  where
  init = do
    pid <- liftEffect $ Raw.spawnLink $ Raw.receiveWithTimeout (Milliseconds 0.0) unit
    pure $ InitOk $ { testPid: pid, receivedExit: false, receivedTerminate: false }

  init2 pid = do
    pure $ InitOk $ { testPid: pid, receivedExit: false, receivedTerminate: false }

  handleInfo (TrappedExit exit) s = do
    pure $ GS2.return $ s { receivedExit = true }

  handleInfo _ _s = do
    unsafeCrashWith "Unexpected message"

  terminate _reason s = do
    liftEffect $ Raw.send s.testPid true

testStartGetSet :: RegistryName TestServerType -> Effect Unit
testStartGetSet registryName = do
  let
    gsSpec :: ServerSpec TestCont TestStop TestMsg _ TestState (ProcessM TestMsg)
    gsSpec =
      (GS2.defaultSpec init)
        { name = Just registryName
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
  (unsafeCoerce serverPid :: Process TestMsg) ! TestMsg -- Trigger HandleInfo to add 100
  getState instanceRef >>= expectState 102 -- Previsouly set state returned
  callContinueReply instanceRef >>= expectState 102 -- Trigger a continue - returning old state
  getState instanceRef >>= expectState 202 -- The continue fires updating state befor the next get
  callContinueNoReply instanceRef >>= expectState 202 -- Tigger a continue that replies in the continuation
  getState instanceRef >>= expectState 302 -- Post the reply, the continuation updates the state
  castContinue instanceRef -- Continues are triggered by casts as well
  getState instanceRef >>= expectState 402 -- As evidenced by the updated state
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

testStopNormal :: RegistryName TestServerType -> Effect Unit
testStopNormal registryName = do
  let
    gsSpec :: ServerSpec TestCont TestStop TestMsg _ TestState (ProcessM TestMsg)
    gsSpec =
      (GS2.defaultSpec init)
        { name = Just registryName
        , handleInfo = Just handleInfo
        , handleContinue = Just handleContinue
        }

    instanceRef = ByName registryName
  void $ crashIfNotStarted <$> (GS2.startLink gsSpec)
  getState instanceRef >>= expectState 0 -- Starts with initial state 0
  -- Try to start the server again - should fail with already running
  (GS2.startLink gsSpec) <#> isAlreadyRunning >>= expect true
  triggerStopCast instanceRef
  sleep 1 -- allow the async cast to execute -- TODO maybe use a monitor with timeout
  void $ crashIfNotStarted <$> (GS2.startLink gsSpec)
  (GS2.startLink gsSpec) <#> isAlreadyRunning >>= expect true
  triggerStopCallReply instanceRef >>= expectState 42 -- New instance starts with initial state 0
  void $ crashIfNotStarted <$> (GS2.startLink gsSpec)
  getState instanceRef >>= expectState 0 -- New instance starts with initial state 0
  -- TODO trigger stop from a handle_info
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

-}
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

getState
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m
  => GS2.ServerRef cont stop appMsg state m -> Effect state
getState handle =
  GS2.call handle callFn
  where
  callFn :: GS2.CallFn state cont stop state m
  callFn _from state =
    pure $ GS2.reply state state


setState
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m
  => GS2.ServerRef cont stop appMsg state m -> state -> Effect state
setState handle newState =
  GS2.call handle callFn
  where
  callFn :: GS2.CallFn state cont stop state m
  callFn _from oldState =
    pure $ GS2.reply oldState newState

setStateCast
  :: forall cont stop msg state m
   . Monad m
  => GS2.ServerRef cont stop msg state m -> state -> Effect Unit
setStateCast handle newState = GS2.cast handle castFn
  where
    castFn :: GS2.CastFn cont stop state m
    castFn state = pure $ GS2.return newState
