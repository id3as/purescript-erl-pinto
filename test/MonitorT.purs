module Test.MonitorT where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Tuple (Tuple, fst)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Process (Process, ProcessM, spawn, toPid)
import Erl.Process.Raw as Raw
import Erl.Test.EUnit (TestF, runTests, suite, test)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (class FFIParseT, class InitialState, class RunT, initialState, receive, runT)
import Pinto.ProcessT.MonitorT (MonitorT, monitor)
import Unsafe.Coerce (unsafeCoerce)

data TestMonitorMsg = TestMonitorMsg
data TestAppMsg = TestAppMsg

testMonitorT  :: Free TestF Unit
testMonitorT =
  suite "MonitorM tests" do
    testMonitor

testMonitor  :: Free TestF Unit
testMonitor =
  test "Start a process and confirm we get a monitor message when it exits" do
    runProcessT theTest
  where

  theTest :: MonitorT TestMonitorMsg (ProcessM Void) Unit
  theTest= do
    pid <- liftEffect $ mySpawn immediatelyExitNormal
    _ <- monitor (toPid pid) $ const TestMonitorMsg
    msg <- receive
    case msg of
      Left TestMonitorMsg -> pure unit
      Right _ ->
        unsafeCrashWith "We got sent a void message!"



bar :: ProcessM Void Int
bar = pure 1


immediatelyExitNormal :: ProcessM Void Unit
immediatelyExitNormal = pure unit



-- runFoo :: MonitorT TestMonitorMsg (ProcessM Void) Unit -> Effect Unit
-- runFoo = runProcessT

runProcessT
  :: forall a state m
   . RunT m state
   => InitialState state
   => m a -> Effect a
runProcessT processT = do
  is <- initialState
  fst <$> runT processT is



mySpawn :: forall m state msg a
         . RunT m state
        => InitialState state
        => FFIParseT m msg
        => m a -> Effect (Process msg)
mySpawn processT = unsafeCoerce $ Raw.spawn $ spawner
  where
    spawner = do
      is <- initialState
      let
        y = runT processT is
      x  <- y
      pure unit
      --void $
