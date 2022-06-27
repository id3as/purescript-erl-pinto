module Test.ProcessT
  ( processTSuite
  ) where

import Prelude

import Bar (MonitorMap, MonitorT(..), monitor)
import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect.Class (liftEffect)
import Erl.Kernel.Erlang (sleep)
import Erl.Process (ProcessM, Process, (!))
import Erl.Process.Raw as Raw
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


data AppMonitorMsg = Boom

processTSuite :: Free TestF Unit
processTSuite =
  suite "Pinto genServer leverages ProcessT" do
    testMonitorT


testMonitorT :: Free TestF Unit
testMonitorT =
  test "HandleInfo receives app messages and monitor message" do
    _ <- pure $ spy "Hello from the mice" {}
    serverPid <- crashIfNotStarted <$> (GS.startLink $ (GS.defaultSpec init) { handleInfo = Just handleInfo })
    _ <- pure $ spy "The pid is" {serverPid}
    sleep (Milliseconds 2000.0)
    pure unit
  where
  init :: InitFn Cont Stop (Either AppMonitorMsg Msg) State (MonitorT AppMonitorMsg (ProcessM Msg))
  -- init :: InitFn (ProcessM Msg) Unit Cont Stop Msg State
  -- init :: ?t
  init = do
    pid <- liftEffect $ Raw.spawn do
      sleep (Milliseconds 1000.0)
    _ <- lift $ monitor pid (const Boom)
    pure $ InitOk $ TestState 0

  --handleInfo :: InfoFn2 Cont Stop Msg State (MonitorT MonitorMsg (ProcessM Msg))
  -- handleInfo :: InfoFn2 ProcessM Unit Cont Stop Msg State
  -- handleInfo :: ?t
  handleInfo msg (TestState x) = do
    case msg of
      Left Boom -> do
        void $ pure $ spy "We are gods walking the earth" msg
      Right _ ->
        pure unit
    pure $ GS.return $ TestState $ x + 1
