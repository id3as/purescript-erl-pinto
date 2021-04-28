module Test.GenServer
  ( genServerSuite
  ) where

import Prelude
import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Process (Process, (!))
import Erl.Process.Raw (Pid)
import Erl.Process.Raw as Raw
import Erl.Test.EUnit (TestF, suite, test)
import Foreign (unsafeToForeign)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.GenServer (Action(..), ExitMessage, From, InitResult(..), ServerRef(..), ServerSpec, ServerType)
import Pinto.GenServer as GS
import Pinto.Types (NotStartedReason(..), RegistryName(..), RegistryReference(..), StartLinkResult, crashIfNotStarted)
import Test.Assert (assertEqual, assert')
import Test.ValueServer as ValueServer
import Unsafe.Coerce (unsafeCoerce)

foreign import sleep :: Int -> Effect Unit

type TestServerType
  = ServerType TestCont TestStop TestMsg TestState

genServerSuite :: Free TestF Unit
genServerSuite =
  suite "Pinto genServer tests" do
    testStartLinkAnonymous
    testStartLinkLocal
    testStartLinkGlobal
    testStopNormalLocal
    testHandleInfo
    testCall
    testCast
    testValueServer
    testTrapExits

data TestState
  = TestState Int

derive instance eqTestState :: Eq TestState

instance showTestState :: Show TestState where
  show (TestState x) = "TestState: " <> show x

data TestCont
  = TestCont
  | TestContFrom (From TestState)

data TestMsg
  = TestMsg
  | TrappedExit ExitMessage

data TestStop
  = StopReason

testStartLinkAnonymous :: Free TestF Unit
testStartLinkAnonymous =
  test "Can start an anonymous GenServer" do
    serverPid <- crashIfNotStarted <$> (GS.startLink $ (GS.defaultSpec init))
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
  init = do
    pure $ InitOk (TestState 0)

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
    serverPid <- crashIfNotStarted <$> (GS.startLink $ (GS.defaultSpec init) { handleInfo = Just handleInfo })
    (unsafeCoerce serverPid :: Process TestMsg) ! TestMsg
    state <- getState (ByPid serverPid)
    assertEqual
      { actual: state
      , expected: TestState 1
      }
    pure unit
  where
  init = do
    pure $ InitOk $ TestState 0

  handleInfo msg (TestState x) = do
    pure $ GS.return $ TestState $ x + 1

testCall :: Free TestF Unit
testCall =
  test "Can create gen_server:call handlers" do
    serverPid <- crashIfNotStarted <$> (GS.startLink $ GS.defaultSpec init)
    state <- getState (ByPid serverPid)
    assertEqual
      { actual: state
      , expected: TestState 7
      }
    pure unit
  where
  init = do
    pure $ InitOk $ TestState 7

testCast :: Free TestF Unit
testCast =
  test "HandleCast changes state" do
    serverPid <- crashIfNotStarted <$> (GS.startLink $ (GS.defaultSpec init))
    setStateCast (ByPid serverPid) $ TestState 42
    state <- getState (ByPid serverPid)
    assertEqual
      { actual: state
      , expected: TestState 42
      }
    pure unit
  where
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

testTrapExits :: Free TestF Unit
testTrapExits =
  suite "Trapped exits" do
    test "Children's exits get translated when requested" do
      serverPid <- crashIfNotStarted <$> (GS.startLink $ (GS.defaultSpec init) { handleInfo = Just handleInfo, trapExits = Just TrappedExit })
      state <- getState (ByPid serverPid)
      assertEqual
        { actual: state.receivedExit
        , expected: true
        }
      pure unit
    test "Parent exits arrive in the terminate callback" do
      testPid <- Raw.self
      void $ Raw.spawnLink $ void $ crashIfNotStarted <$> (GS.startLink $ (GS.defaultSpec $ init2 testPid) { terminate = Just terminate, trapExits = Just TrappedExit })
      receivedTerminate <- Raw.receiveWithTimeout 500 false
      assert' "Terminate wasn't called on the genserver" receivedTerminate
  where
  init = do
    pid <- GS.lift $ Raw.spawnLink $ Raw.receiveWithTimeout 0 unit
    pure $ InitOk $ { testPid: pid, receivedExit: false, receivedTerminate: false }

  init2 pid = do
    pure $ InitOk $ { testPid: pid, receivedExit: false, receivedTerminate: false }

  handleInfo (TrappedExit exit) s = do
    pure $ GS.return $ s { receivedExit = true }

  handleInfo _ s = do
    unsafeCrashWith "Unexpected message"

  terminate reason s = do
    GS.lift $ Raw.send s.testPid true

---------------------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------------------
testStartGetSet :: RegistryName TestServerType -> Effect Unit
testStartGetSet registryName = do
  let
    gsSpec :: ServerSpec TestCont TestStop TestMsg TestState
    gsSpec =
      (GS.defaultSpec init)
        { name = Just registryName
        , handleInfo = Just handleInfo
        , handleContinue = Just handleContinue
        }

    instanceRef = ByName registryName
  serverPid <- crashIfNotStarted <$> (GS.startLink gsSpec)
  getState instanceRef >>= expect 0 -- Starts with initial state 0
  setState instanceRef (TestState 1) >>= expect 0 -- Set new as 1, old is 0
  getState instanceRef >>= expect 1 -- Previsouly set state returned
  setStateCast instanceRef (TestState 2) -- Set new state async
  getState instanceRef >>= expect 2 -- Previsouly set state returned
  (unsafeCoerce serverPid :: Process TestMsg) ! TestMsg -- Trigger HandleInfo to add 100
  getState instanceRef >>= expect 102 -- Previsouly set state returned
  callContinueReply instanceRef >>= expect 102 -- Trigger a continue - returning old state
  getState instanceRef >>= expect 202 -- The continue fires updating state befor the next get
  callContinueNoReply instanceRef >>= expect 202 -- Tigger a continue that replies in the continuation
  getState instanceRef >>= expect 302 -- Post the reply, the continuation updates the state
  castContinue instanceRef -- Continues are triggered by casts as well
  getState instanceRef >>= expect 402 -- As evidenced by the updated state
  pure unit
  where
  init = do
    pure $ InitOk (TestState 0)

  handleInfo TestMsg (TestState x) = do
    pure $ GS.return $ TestState $ x + 100

  handleInfo _ s = do
    unsafeCrashWith "Unexpected message"

  handleContinue cont (TestState x) =
    GS.lift do
      case cont of
        TestCont -> pure $ GS.return $ TestState $ x + 100
        TestContFrom from -> do
          GS.replyTo from (TestState x)
          pure $ GS.return $ TestState $ x + 100

  callContinueReply handle = GS.call handle \_from state -> pure $ GS.replyWithAction state (Continue TestCont) state

  callContinueNoReply handle = GS.call handle \from state -> pure $ GS.noReplyWithAction (Continue $ TestContFrom from) state

  castContinue handle = GS.cast handle \state -> pure $ GS.returnWithAction (Continue TestCont) state

testStopNormal :: RegistryName TestServerType -> Effect Unit
testStopNormal registryName = do
  let
    gsSpec :: ServerSpec TestCont TestStop TestMsg TestState
    gsSpec =
      (GS.defaultSpec init)
        { name = Just registryName
        , handleInfo = Just handleInfo
        , handleContinue = Just handleContinue
        }

    instanceRef = ByName registryName
  serverPid <- crashIfNotStarted <$> (GS.startLink gsSpec)
  getState instanceRef >>= expect 0 -- Starts with initial state 0
  -- Try to start the server again - should fail with already running
  (GS.startLink gsSpec) <#> isAlreadyRunning >>= expectBool true
  triggerStopCast instanceRef
  sleep 1 -- allow the async cast to execute -- TODO maybe use a monitor with timeout
  void $ crashIfNotStarted <$> (GS.startLink gsSpec)
  (GS.startLink gsSpec) <#> isAlreadyRunning >>= expectBool true
  triggerStopCallReply instanceRef >>= expect 42 -- New instance starts with initial state 0
  void $ crashIfNotStarted <$> (GS.startLink gsSpec)
  getState instanceRef >>= expect 0 -- New instance starts with initial state 0
  -- TODO trigger stop from a handle_info
  pure unit
  where
  init = do
    pure $ InitOk (TestState 0)

  handleInfo TestMsg (TestState x) = do
    pure $ GS.return $ TestState $ x + 100

  handleInfo _ _ = do
    unsafeCrashWith "Unexpected message"

  handleContinue cont (TestState x) =
    GS.lift do
      case cont of
        TestCont -> pure $ GS.return $ TestState $ x + 100
        TestContFrom from -> do
          GS.replyTo from (TestState x)
          pure $ GS.return $ TestState $ x + 100

  callContinueReply handle = GS.call handle \_from state -> pure $ GS.replyWithAction state (Continue TestCont) state

  callContinueNoReply handle = GS.call handle \from state -> pure $ GS.noReplyWithAction (Continue $ TestContFrom from) state

  castContinue handle = GS.cast handle \state -> pure $ GS.returnWithAction (Continue TestCont) state

  triggerStopCast handle = GS.cast handle \state -> pure $ GS.returnWithAction StopNormal state

  triggerStopCallReply handle = GS.call handle \from state -> pure $ GS.replyWithAction (TestState 42) StopNormal state

isAlreadyRunning :: forall serverType. StartLinkResult serverType -> Boolean
isAlreadyRunning = case _ of
  Left (AlreadyStarted _) -> true
  _ -> false

expect :: Int -> TestState -> Effect Unit
expect expected actual = assertEqual { actual, expected: TestState expected }

expectBool :: Boolean -> Boolean -> Effect Unit
expectBool expected actual = assertEqual { actual, expected: expected }

getState :: forall cont stop msg state. ServerRef cont stop msg state -> Effect state
getState handle =
  GS.call handle \_from state ->
    let
      reply = state
    in
      pure $ GS.reply reply state

setState :: forall cont stop msg state. ServerRef cont stop msg state -> state -> Effect state
setState handle newState =
  GS.call handle \_from state ->
    let
      reply = state
    in
      pure $ GS.reply reply newState

setStateCast :: forall cont stop msg state. ServerRef cont stop msg state -> state -> Effect Unit
setStateCast handle newState = GS.cast handle \_state -> pure $ GS.return newState
