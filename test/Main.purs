module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Erl.Test.EUnit (TestF, runTests, suite, test)
import Pinto.GenServer (CallResult(..), ServerRunning(..), ServerType)
import Pinto.GenServer as GS
import Pinto.Sup (ChildShutdownTimeoutStrategy(..), ChildSpec, ChildType(..), RestartStrategy(..), Strategy(..), SupervisorSpec, mkErlChildSpec)
import Pinto.Sup as Sup
import Pinto.Types (InstanceRef(..), RegistryName(..), crashIfNotStarted)
import Test.Assert (assertEqual)
import Test.GenServer as TGS
import Unsafe.Coerce (unsafeCoerce)

foreign import filterSasl :: Effect  Unit

main :: Effect Unit
main =
  let _ = unsafePerformEffect filterSasl
  in
    void $ runTests do
      TGS.genServerSuite
      supervisorSuite

supervisorSuite :: Free TestF Unit
supervisorSuite =
  suite "Pinto supervisot tests" do
    testStartWithNamedChild


data TestState = TestState Int
derive instance eqTestState :: Eq TestState
instance showTestState :: Show TestState where
  show (TestState x) = "TestState: " <> show x

data TestCont = TestCont
data TestMsg = TestMsg




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

      --myChild :: ChildSpec String TestState TestMsg
      myChild = mkChildSpec "myChildId" (GS.startLink $ (GS.mkSpec childInit) { name = Just childName })


--mkChildSpec :: forall childType. String -> ChildSpec childType
mkChildSpec id start  = { id
                  , childType : Worker
                  , start
                  , restartStrategy: RestartOnCrash
                  , shutdownStrategy: KillAfter 5000
                  }

---------------------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------------------
getState :: forall cont stop msg state. InstanceRef (ServerType cont stop msg state) ->  Effect state
getState handle = GS.call handle
  \_from state ->
    let reply = state
    in pure $ GS.reply reply state

setState :: forall cont stop msg state. InstanceRef (ServerType cont stop msg state) -> state ->  Effect state
setState handle newState = GS.call handle
  \_from state ->
    let reply = state
    in pure $ GS.reply reply newState


setStateCast :: forall cont stop msg state. InstanceRef (ServerType cont stop msg state) -> state ->  Effect Unit
setStateCast handle newState = GS.cast handle
  \_state -> pure $ GS.return newState
