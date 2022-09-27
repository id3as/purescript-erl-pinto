module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Kernel.Application (ensureAllStarted)
import Erl.Process (ProcessM)
import Erl.Test.EUnit (TestF, runTests, suite, test)
import Pinto (StartLinkResult)
import Pinto.GenServer2 (InitFn, InitResult(..), ServerPid)
import Pinto.GenServer2 as GS2
import Pinto.Supervisor (ChildShutdownTimeoutStrategy(..), ChildType(..), RestartStrategy(..), Strategy(..), SupervisorSpec, ChildSpec, spec)
import Pinto.Supervisor as Sup
import Pinto.Supervisor.SimpleOneForOne as DynamicSup
import Pinto.Types (RegistryName(..), RegistryReference(..), crashIfNotStarted)
import Test.Assert (assertEqual)
import Test.BusT (testBusT)
import Test.DoorLock as DoorLock
import Test.GenServer as TGS
import Test.GenServer2 as TGS2
import Test.MetadataBusT (testMetadataBusT)
import Test.MonitorT (testMonitorT)
import Test.StateBusT (testStateBusT)
import Test.TestHelpers (getState)
import Test.TrapExitT (testTrapExitT)
import Test.ValueServer (testValueServer)

foreign import filterSasl :: Effect Unit

main :: Effect Unit
main = do
  filterSasl
  void $ ensureAllStarted $ atom "gproc"
  void $ runTests do
    -- TGS.genServerSuite
    -- TGS2.genServer2Suite
    -- testMonitorT
    -- testTrapExitT
    -- testBusT
    testStateBusT
    testMetadataBusT

-- testValueServer
-- DoorLock.testSuite
-- supervisorSuite

supervisorSuite :: Free TestF Unit
supervisorSuite =
  suite "Pinto supervisor tests" do
    testStartWithNamedChild
    dynamicSupervisor

data TestState = TestState Int

derive instance eqTestState :: Eq TestState

instance showTestState :: Show TestState where
  show (TestState x) = "TestState: " <> show x

data TestCont = TestCont

data TestMsg = TestMsg

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

  childInit :: InitFn TestState (ProcessM Void)
  childInit = do
    pure $ InitOk $ TestState 0

  childName = Local $ atom "testNamedChild"

  --myChild :: ChildSpec String TestState TestMsg
  myChild = mkChildSpec "myChildId" (GS2.startLink $ (GS2.defaultSpec childInit) { serverName = Just childName })

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
  supInit :: Effect (DynamicSup.ChildSpec Unit (ServerPid TestState (ProcessM Void)))
  supInit =
    pure
      { intensity: 1
      , period: Seconds 5.0
      , childType: Worker
      , start: childStart
      , restartStrategy: RestartTemporary
      , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
      }

  childStart _ = GS2.startLink $ (GS2.defaultSpec childInit)

  childInit :: InitFn TestState (ProcessM Void)
  childInit = do
    pure $ InitOk $ TestState 0
