module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, nil, (:))
import Erl.Process (Process(..), (!))
import Erl.Process.Raw (Pid)
import Erl.Test.EUnit (TestF, TestSet, collectTests, runTests, suite, test)
import Partial.Unsafe (unsafePartial)
import Pinto.GenServer (CallResult(..), CastResult(..), InitFn, ServerRunning(..))
import Pinto.GenServer as GS
import Pinto.Sup (ChildShutdownTimeoutStrategy(..), ChildSpec, ChildType(..), RestartStrategy(..), Strategy(..), SupervisorSpec, mkErlChildSpec)
import Pinto.Sup as Sup
import Pinto.Types (InstanceRef(..), NotStartedReason(..), RegistryName(..), ServerPid, StartLinkResult, crashIfNotStarted)
import Test.Assert (assert, assertEqual)
import Unsafe.Coerce (unsafeCoerce)

foreign import filterSasl :: Effect  Unit

main :: Effect Unit
main =
  let _ = unsafePerformEffect filterSasl
  in
    void $ runTests do
      genServerSuite

genServerSuite :: Free TestF Unit
genServerSuite =
  suite "Pinto genServer test" do
    testStartLinkAnonymous
    testStartLinkLocal
    testHandleInfo
    testCall
    testCast

    testStartWithNamedChild


data TestState = TestState Int
derive instance eqTestState :: Eq TestState
instance showTestState :: Show TestState where
  show (TestState x) = "TestState: " <> show x

data TestCont = TestCont
data TestMsg = TestMsg

testStartLinkAnonymous :: Free TestF Unit
testStartLinkAnonymous =
  test "Can start an anonymous GenServer" do
    slRes <- GS.startLink $ GS.mkSpec init
    let
      worked = case slRes of
        Right pid -> true
        Left reason -> false
    assert worked
    pure unit

    where
      init = do
        pure $ Right $ InitOk $ TestState 0

testStartLinkLocal :: Free TestF Unit
testStartLinkLocal =
  test "Can start a locally named GenServer" do
    slRes <- GS.startLink $ (GS.mkSpec init) { name = Just (Local (atom "foo")) }
    let
      worked = case slRes of
        Right pid -> true
        Left reason -> false
    assert worked
    pure unit

    where
      init = do
        pure $ Right $ InitOk (TestState 0)

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




--------------------------------------------------------------------------------
-- Supervisor Test
--------------------------------------------------------------------------------
testStartWithNamedChild :: Free TestF Unit
testStartWithNamedChild =
  test "Can start a supervisor with a single named child" do
    supPid <- crashIfNotStarted <$> Sup.startLink Nothing supInit

    childState <- getState $ ByName childName
    assertEqual { actual: childState
                , expected: TestState 0
                }


    pure unit

    where
      childSpecs = mkErlChildSpec myChild
                   : nil

      supInit :: Effect SupervisorSpec
      supInit =
        pure { flags: { strategy : OneForOne
                      , intensity : 1
                      , period: 5
                      }
             , childSpecs
             }

      childInit = do
        pure $ Right $ InitOk $ TestState 0

      childName = Local $ atom "testNamedChild"

      myChild :: ChildSpec String TestState TestMsg
      myChild = (mkChildSpec "myChildId")
                  { start = GS.startLink $ (GS.mkSpec childInit) { name = Just childName }
                  }


data SupStateExample = SupStateExample





mkChildSpec :: forall childState childMsg. String -> ChildSpec String childState childMsg
mkChildSpec id  = { id
                  , childType : Worker
                  , start : unsafeCoerce unit
                  , restartStrategy: RestartOnCrash
                  , shutdownStrategy: KillAfter 5000
                  }

---------------------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------------------
getState :: forall state msg. InstanceRef state msg -> Effect state
getState handle = GS.call handle
       \state -> pure $ CallReply state state


setState :: forall state msg. InstanceRef state msg -> state ->  Effect state
setState handle newState = GS.call handle
       \state -> pure $ CallReply state newState


setStateCast :: forall state msg. InstanceRef state msg -> state ->  Effect Unit
setStateCast handle newState = GS.cast handle
       \_state -> pure $ NoReply newState
