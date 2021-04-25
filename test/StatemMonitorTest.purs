module Test.StatemMonitorTest
  ( testSuite
  , startLink
  , State
  , StateId
  , TimerContent
  , OurPid
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Pinto.GenStatem (class HasStateId, EventResult(..), InitResult(..), MonitorRef, StatemPid, StatemRef(..), StatemType)
import Pinto.GenStatem as Statem
import Pinto.Types (RegistryName(..), crashIfNotStarted)
import Erl.Process.Raw (class HasPid)
import Debug.Trace (spy)
import Test.ValueServer (ValueServerPid)
import Test.ValueServer as ValueServer
-- Test-specific imports
import Control.Monad.Free (Free)
import Erl.Test.EUnit (TestF, suite, test)
import Test.Assert (assertEqual)

-- -----------------------------------------------------------------------------
-- Test Implementation
-- -----------------------------------------------------------------------------
testSuite :: Free TestF Unit
testSuite =
  suite "Pinto GenStatem Monitor Tests" do
    test "Monitor from Init Works" do
      valueServerPid <- ValueServer.startLink
      monitorPid <- startLink valueServerPid
      downReceived1 <- downReceived
      assertEqual { actual: downReceived1, expected: false }
      ValueServer.stop
      downReceived2 <- downReceived
      assertEqual { actual: downReceived2, expected: true }
      pure unit

-- -----------------------------------------------------------------------------
-- Statem Implementation
-- -----------------------------------------------------------------------------
data StateId
  = StateMain

derive instance eqStateId :: Eq StateId

newtype State
  = State
  {
  }

instance stateHasStateId :: HasStateId StateId State where
  getStateId _ = StateMain

type Data
  = { monitorRef :: MonitorRef
    , downReceived :: Boolean
    }

type Info
  = Void

type Internal
  = Void

type TimerName
  = Void

data TimerContent
  = Void

type OurType
  = StatemType Info Internal TimerName TimerContent Data StateId State

newtype OurPid
  = OurPid (StatemPid Info Internal TimerName TimerContent Data StateId State)

-- Only surface the raw pid, don't implement HasProcess - we don't want folks sending us messages using our Info
-- type
derive newtype instance ourPidHasPid :: HasPid OurPid

name :: RegistryName OurType
name = Local $ atom "statemMonitorTest"

startLink :: ValueServerPid -> Effect OurPid
startLink valueServerPid = do
  OurPid <$> crashIfNotStarted <$> (Statem.startLink $ ((Statem.mkSpec init handleEvent) { name = Just name }))
  where
  init = do
    monitorRef <- Statem.monitor valueServerPid handleValueServerStopped
    let
      initialState = State {}
    let
      initialData = { monitorRef, downReceived: false }
    pure $ InitOk initialState initialData

  handleEvent event _state _data = do
    pure $ EventKeepStateAndData

  handleValueServerStopped reason _state commonData = do
    let
      _ = spy "Down Reason" reason
    pure $ EventKeepState (commonData { downReceived = true })

downReceived :: Effect Boolean
downReceived = Statem.call (ByName name) impl
  where
  impl from _state { downReceived } = do
    let
      actions = Statem.newActions # Statem.addReply (Statem.mkReply from downReceived)
    pure $ EventKeepStateAndDataWithActions actions
