module Test.StateBusT
  ( testStateBusT
  ) where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Erl.Process (ProcessM)
import Erl.Test.EUnit (TestF, suite)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (receive)
import Pinto.ProcessT.BusT.StateBusT (class UpdateState, Bus, BusMsg(..), BusRef, StateBusT, busRef, create, raise, subscribe)
import Test.TestHelpers (mpTest)

type TestBus = Bus String TestBusMsg TestBusState
type TestBusRef = BusRef String TestBusMsg TestBusState
data TestBusMsg = TestBusMsg
data TestBusState = TestBusState Int
data TestMappedMsg = TestMappedMsg

instance UpdateState TestBusState TestBusMsg where
  updateState TestBusMsg (TestBusState i) = TestBusState (i + 1)

data TestAppMsg = TestAppMsg
data TestTimeoutMsg = TestTimeoutMsg

-- foreign import sendSelfLateMessage :: forall name msg. Bus name msg  -> msg -> Milliseconds -> Effect Unit

testStateBusT :: Free TestF Unit
testStateBusT =
  suite "BusM tests" do
    testInitialStateSubscribeThenCreate

-- TODO testInitialStateCreateTheSubscribe - will fail :)

testInitialStateSubscribeThenCreate :: Free TestF Unit
testInitialStateSubscribeThenCreate =
  mpTest "If you subscribe before a bus is created you get initial state" theTest
  where

  theTest :: StateBusT (BusMsg TestBusMsg TestBusState) (ProcessM Void) Unit
  theTest = do
    subscribe testBusRef identity
    testBus <- createTestBus
    raiseBusMessage testBus

    msg <- receive
    case msg of
      Left (State (TestBusState i))
        | i == 0 ->
            pure unit
        | otherwise ->
            unsafeCrashWith $ "Initial state (" <> show i <> ") wrong!"
      Left _ ->
        unsafeCrashWith "We got sent the wrong message!"
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

{-
testMapMsg :: Free TestF Unit
testMapMsg =
  mpTest "We receive mapped messages" theTest
  where

  theTest :: BusT TestMappedMsg (ProcessM Void) Unit
  theTest = do
    subscribe testBus mapper
    void $ liftEffect $ spawn raiseBusMessage

    msg <- receive
    case msg of
      Left TestMappedMsg -> pure unit
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

  mapper TestBusMsg = TestMappedMsg

testUnsubscribe :: Free TestF Unit
testUnsubscribe =
  mpTest "No longer receive messages after unsubscribe" theTest
  where

  theTest :: BusT TestBusMsg (ProcessM Void) Unit
  theTest = do
    subscribe testBus identity
    void $ liftEffect $ spawn raiseBusMessage

    msg <- receive
    case msg of
      Left TestBusMsg -> do
        unsubscribe testBus
        void $ liftEffect $ spawn raiseBusMessage
        msg2 <- receiveWithTimeout (Milliseconds 10.0)
        case msg2 of
          Left Timeout ->
            pure unit
          Right _ ->
            unsafeCrashWith "Message after unsubscribe"
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

testMessageAfterUnsubscribe :: Free TestF Unit
testMessageAfterUnsubscribe =
  mpTest "Swallow messages after unsubscribe" theTest
  where

  theTest :: BusT TestBusMsg (ProcessM Void) Unit
  theTest = do
    subscribe testBus identity
    void $ liftEffect $ spawn raiseBusMessage

    msg <- receive
    case msg of
      Left TestBusMsg -> do
        unsubscribe testBus
        liftEffect $ sendSelfLateMessage testBus TestBusMsg (Milliseconds 8.0)
        (Milliseconds preRWT) <- liftEffect milliseconds
        msg2 <- receiveWithTimeout (Milliseconds 10.0)
        (Milliseconds postRWT) <- liftEffect milliseconds
        case msg2 of
          Left Timeout -> do
            let duration = postRWT - preRWT
            checkDuration duration
          Right _ ->
            unsafeCrashWith "Message after unsubscribe"
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

  checkDuration duration
    | duration < 9.0 = unsafeCrashWith "Message timeout too short"
    | duration > 11.0 = unsafeCrashWith "Message timeout too long"
    | otherwise = pure unit

testMultipleBusses :: Free TestF Unit
testMultipleBusses =
  mpTest "Can subscribe to multiple busses - each with their own mapper" theTest
  where

  theTest :: BusT Int (ProcessM Void) Unit
  theTest = do
    subscribe testBus (const 1)
    subscribe testBus2 (const 10)
    void $ liftEffect $ spawn raiseBusMessage
    void $ liftEffect $ spawn raiseBusMessage2

    int1 <- doReceive
    int2 <- doReceive
    liftEffect $ assertEqual { actual: int1 + int2, expected: 11 }

  doReceive = do
    msg <- receive
    case msg of
      Left x -> pure x
      Right _ ->
        unsafeCrashWith "We got sent a void message!"
-}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
--raiseBusMessage :: ProcessM Void Unit
raiseBusMessage :: TestBus -> StateBusT (BusMsg TestBusMsg TestBusState) (ProcessM Void) Unit
raiseBusMessage testBus = do
  liftEffect $ raise testBus TestBusMsg

--raiseBusMessage2 :: ProcessM Void Unit
raiseBusMessage2 :: forall t22 name24 state26. MonadEffect t22 => UpdateState state26 TestBusMsg => Bus name24 TestBusMsg state26 -> t22 Unit
raiseBusMessage2 testBus = do
  liftEffect $ raise testBus TestBusMsg

createTestBus :: forall t37. MonadEffect t37 => t37 (Bus String TestBusMsg TestBusState)
createTestBus = liftEffect $ create testBusRef (TestBusState 0)

testBusRef :: TestBusRef
testBusRef = busRef "TestBus"

--testBusRef2 :: BusRef Atom TestBusMsg TestBusState
--testBusRef2 = busRef $ atom "TestBus2"
