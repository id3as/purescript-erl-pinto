module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Process (ProcessM)
import Erl.Test.EUnit (TestF, runTests, suite, test)
import Pinto (StartLinkResult)
import Pinto.GenServer2 (InitResult(..), ServerPid, ServerRef, InitFn)
import Pinto.GenServer2 as GS2
import Pinto.Supervisor (ChildShutdownTimeoutStrategy(..), ChildType(..), RestartStrategy(..), Strategy(..), SupervisorSpec, ChildSpec, spec)
import Pinto.Supervisor as Sup
import Pinto.Supervisor.SimpleOneForOne as DynamicSup
import Pinto.Types (RegistryName(..), RegistryReference(..), crashIfNotStarted)
import Test.Assert (assertEqual)
import Test.DoorLock as DoorLock
import Test.GenServer as TGS
import Test.GenServer2 as TGS2
import Test.MonitorT (testMonitorT)

foreign import filterSasl :: Effect Unit

main :: Effect Unit
main =
  let
    _ = unsafePerformEffect filterSasl
  in
    void
      $ runTests do
          TGS.genServerSuite
          TGS2.genServer2Suite
          testMonitorT

          --DoorLock.testSuite
          --supervisorSuite

supervisorSuite :: Free TestF Unit
supervisorSuite =
  suite "Pinto supervisor tests" do
    testStartWithNamedChild
    dynamicSupervisor

data TestState
  = TestState Int

derive instance eqTestState :: Eq TestState

instance showTestState :: Show TestState where
  show (TestState x) = "TestState: " <> show x

data TestCont
  = TestCont

data TestMsg
  = TestMsg

--------------------------------------------------------------------------------
-- Standard Supervisor Test
--------------------------------------------------------------------------------
testStartWithNamedChild :: Free TestF Unit
testStartWithNamedChild =
  test "Can start a supervisor with a single named child" do
    _supPid <- crashIfNotStarted <$> Sup.startLink Nothing supInit
    childState <- getState $ ByName childName
    assertEqual
      { actual: childState
      , expected: TestState 0
      }
    pure unit
  where
  childSpecs =
    spec myChild
      : nil

  supInit :: Effect SupervisorSpec
  supInit =
    pure
      { flags:
          { strategy: OneForOne
          , intensity: 1
          , period: Seconds 5.0
          }
      , childSpecs
      }
  childInit :: InitFn _ _  (ProcessM Void)
  childInit = do
    pure $ InitOk $ TestState 0

  childName = Local $ atom "testNamedChild"

  --myChild :: ChildSpec String TestState TestMsg
  myChild = mkChildSpec "myChildId" (GS2.startLink3 $ (GS2.defaultSpec childInit) { name = Just childName })

mkChildSpec :: forall childType. String -> Effect (StartLinkResult childType) -> ChildSpec childType
mkChildSpec id start =
  { id
  , childType: Worker
  , start
  , restartStrategy: RestartTemporary
  , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
  }

--------------------------------------------------------------------------------
-- Dynamic Supervisor Test
--------------------------------------------------------------------------------
dynamicSupervisor :: Free TestF Unit
dynamicSupervisor =
  test "Can start a supervisor and add a child" do
    supPid <- crashIfNotStarted <$> DynamicSup.startLink Nothing supInit
    childPid <- Sup.crashIfChildNotStarted <$> DynamicSup.startChild (ByPid supPid) unit
    childState <- getState (ByPid childPid)
    assertEqual
      { actual: childState
      , expected: TestState 0
      }
    pure unit
  where
  supInit :: Effect (DynamicSup.ChildSpec Unit (ServerPid Void Void TestState (ProcessM Void)))
  supInit =
    pure
      { intensity: 1
      , period: Seconds 5.0
      , childType: Worker
      , start: childStart
      , restartStrategy: RestartTemporary
      , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
      }

  childStart _ = GS2.startLink3 $ (GS2.defaultSpec childInit)

  childInit :: InitFn _ _ (ProcessM Void)
  childInit = do
    pure $ InitOk $ TestState 0

---------------------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------------------
getState :: forall cont stop msg state. ServerRef cont stop state (ProcessM msg) -> Effect state
getState handle =
  GS2.call handle \_from state ->
    let
      reply = state
    in
      pure $ GS2.reply reply state

setState :: forall cont stop msg state. ServerRef cont stop state (ProcessM msg) -> state -> Effect state
setState handle newState =
  GS2.call handle \_from state ->
    let
      reply = state
    in
      pure $ GS2.reply reply newState

setStateCast :: forall cont stop msg state . ServerRef cont stop state (ProcessM (msg)) -> state -> Effect Unit
setStateCast handle newState = GS2.cast handle \_state -> pure $ GS2.return newState
