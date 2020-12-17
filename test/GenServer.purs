module Test.GenServer
  (  genServerSuite
--  ,  testStartLinkAnonymous
  , testStartLinkLocal
  , testHandleInfo
  , testCall
  , testCast
  , testValueServer
  ) where


import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process (Process, (!))
import Erl.Test.EUnit (TestF, suite, test)
import Foreign (unsafeToForeign)
import Pinto.GenServer (CallResult(..), CastResult(..), ServerRunning(..))
import Pinto.GenServer as GS
import Pinto.Types (InstanceRef(..), RegistryName(..), crashIfNotStarted)
import Test.Assert (assertEqual)
import Test.ValueServer as ValueServer
import Unsafe.Coerce (unsafeCoerce)



foreign import startGprocFFI :: Effect Unit

genServerSuite :: Free TestF Unit
genServerSuite =
  suite "Pinto genServer tests" do
    testStartLinkAnonymous
    testStartLinkLocal
    testStartLinkGlobal
    -- testStartLinkVia
    testHandleInfo
    testCall
    testCast


    testValueServer

data TestState = TestState Int
derive instance eqTestState :: Eq TestState
instance showTestState :: Show TestState where
  show (TestState x) = "TestState: " <> show x

data TestCont = TestCont
data TestMsg = TestMsg

testStartLinkAnonymous :: Free TestF Unit
testStartLinkAnonymous =
  test "Can start an anonymous GenServer" do
    serverPid <- crashIfNotStarted <$> (GS.startLink $ (GS.mkSpec init))
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
      pure $ Right $ InitOk (TestState 0)

testStartLinkLocal :: Free TestF Unit
testStartLinkLocal =
  test "Can start a locally named GenServer" do
    testStartGetSet $ Local $ atom "testStartLinkLocal"

testStartLinkGlobal :: Free TestF Unit
testStartLinkGlobal =
  test "Can start a globally named GenServer" do
    testStartGetSet $ Global (unsafeToForeign $  atom "testStartLinkGlobal")

-- testStartLinkVia :: Free TestF Unit
-- testStartLinkVia =
--   test "Can start a gproc named GenServer" do
--     startGprocFFI
--     let
--       viaName = Via (NativeModuleName $ atom "gproc") $ unsafeToForeign (tuple3 (atom "n") (atom "l") "testStartLinkVia")
--     testStartGetSet viaName


testHandleInfo :: Free TestF Unit
testHandleInfo =
  test "HandleInfo handler receives message" do
    serverPid <- crashIfNotStarted <$> (GS.startLink $ (GS.mkSpec init) { handleInfo = Just handleInfo })

    (unsafeCoerce serverPid :: Process TestMsg) ! TestMsg

    state <- getState (ByPid serverPid)
    assertEqual { actual: state
                , expected: TestState 1
                }

    pure unit

    where
      init = do
        pure $ Right $ InitOk $ TestState 0

      handleInfo msg (TestState x) = do
        let _ = spy "Got message" msg
        pure $ NoReply $ TestState $ x + 1


testCall :: Free TestF Unit
testCall =
  test "Can create gen_server:call handlers" do
    serverPid <- crashIfNotStarted  <$> (GS.startLink $ GS.mkSpec init)

    state <- getState (ByPid serverPid)
    assertEqual { actual: state
                , expected: TestState 7
                }
    pure unit

    where
      init = do
        pure $ Right $ InitOk $ TestState 7


testCast :: Free TestF Unit
testCast =
  test "HandleCast changes state" do
    serverPid <- crashIfNotStarted <$> (GS.startLink $ (GS.mkSpec init))

    setStateCast (ByPid serverPid) $ TestState 42

    state <- getState (ByPid serverPid)
    assertEqual { actual: state
                , expected: TestState 42
                }
    pure unit

    where
      init = do
        pure $ Right $ InitOk $ TestState 0

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



---------------------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------------------
testStartGetSet registryName = do
  let
    instanceRef = ByName registryName
  void $ crashIfNotStarted <$> (GS.startLink $ (GS.mkSpec init) { name = Just registryName })
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
      pure $ Right $ InitOk (TestState 0)





getState :: forall state msg. InstanceRef state msg -> Effect state
getState handle = GS.call handle
       \from state -> pure $ CallReply state state


setState :: forall state msg. InstanceRef state msg -> state ->  Effect state
setState handle newState = GS.call handle
       \from state -> pure $ CallReply state newState


setStateCast :: forall state msg. InstanceRef state msg -> state ->  Effect Unit
setStateCast handle newState = GS.cast handle
       \_state -> pure $ NoReply newState
