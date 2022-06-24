module Test.ProcessT
  ( processTSuite
  ) where

import Prelude

import Bar (MonitorT(..), ProcessM, MonitorMap)
import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Erl.Process (Process, (!))
import Erl.Test.EUnit (TestF, suite, test)
import Pinto (crashIfNotStarted)
import Pinto.GenServer (InfoFn2, InitFn, InitResult(..), ServerSpec)
import Pinto.GenServer as GS
import Unsafe.Coerce (unsafeCoerce)


data State
  = TestState Int

derive instance Eq State

instance showTestState :: Show State where
  show (TestState x) = "TestState: " <> show x

type Cont
  = Void

type Stop
  = Void

data Msg
  = TestMsg


data MonitorMsg = Boom

processTSuite :: Free TestF Unit
processTSuite =
  suite "Pinto genServer leverages ProcessT" do
    testMonitorT


testMonitorT :: Free TestF Unit
testMonitorT =
  test "HandleInfo receives app messages and monitor message" do
    let
      --x :: ServerSpec (MonitorT MonitorMsg (ProcessM Msg)) Cont Stop Msg State
      x = (GS.defaultSpec init) { handleInfo = Just handleInfo }

    serverPid <- crashIfNotStarted <$> (GS.startLink $ x)
    pure unit
  where
  init :: InitFn (MonitorT MonitorMsg (ProcessM Unit)) Cont Stop Msg State
  -- init :: InitFn (ProcessM Msg) Unit Cont Stop Msg State
  -- init :: ?t
  init = do
    unsafeCoerce 1
    --pure $ InitOk $ TestState 0

  --handleInfo :: InfoFn2 (MonitorT MonitorMsg (ProcessM Msg)) Cont Stop Msg State
  -- handleInfo :: InfoFn2 ProcessM Unit Cont Stop Msg State
  -- handleInfo :: ?t
  handleInfo msg (TestState x) = do
    unsafeCoerce 1
    -- pure $ GS.return $ TestState $ x + 1
