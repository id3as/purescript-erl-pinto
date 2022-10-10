module Test.BusT
  ( testBusT
  ) where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Kernel.Time (milliseconds)
import Erl.Test.EUnit (TestF, suite)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (Timeout(..), receive, receiveWithTimeout, spawn, ProcessTM)
import Pinto.ProcessT.BusT (Bus, BusT, bus, raise, subscribe, unsubscribe)
import Test.Assert (assertEqual)
import Test.TestHelpers (mpTest)

data TestBusMsg = TestBusMsg
data TestMappedMsg = TestMappedMsg

data TestAppMsg = TestAppMsg
data TestTimeoutMsg = TestTimeoutMsg

foreign import sendSelfLateMessage :: forall name msg. Bus name msg -> msg -> Milliseconds -> Effect Unit

testBusT :: Free TestF Unit
testBusT =
  suite "BusM tests" do
    testReceiveMsg
    testMapMsg
    testUnsubscribe
    testMessageAfterUnsubscribe
    testMultipleBusses

testReceiveMsg :: Free TestF Unit
testReceiveMsg =
  mpTest "Can send and receive messages" theTest
  where

  theTest :: BusT TestBusMsg (ProcessTM Void _) Unit
  theTest = do
    subscribe testBus identity
    void $ liftEffect $ spawn raiseBusMessage

    msg <- receive
    case msg of
      Left TestBusMsg -> pure unit
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

testMapMsg :: Free TestF Unit
testMapMsg =
  mpTest "We receive mapped messages" theTest
  where

  theTest :: BusT TestMappedMsg (ProcessTM Void _) Unit
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

  theTest :: BusT TestBusMsg (ProcessTM Void _) Unit
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

  theTest :: BusT TestBusMsg (ProcessTM Void _) Unit
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

  theTest :: BusT Int (ProcessTM Void _) Unit
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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
raiseBusMessage :: forall handledMsg. ProcessTM Void handledMsg Unit
raiseBusMessage = do
  liftEffect $ raise testBus TestBusMsg

raiseBusMessage2 :: forall handledMsg. ProcessTM Void handledMsg Unit
raiseBusMessage2 = do
  liftEffect $ raise testBus2 TestBusMsg

testBus :: Bus String TestBusMsg
testBus = bus "TestBus"

testBus2 :: Bus Atom TestBusMsg
testBus2 = bus $ atom "TestBus2"
