module Test.StateBusT
  ( testStateBusT
  ) where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Process (ProcessM)
import Erl.Test.EUnit (TestF, suite)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (Timeout(..), receive, receiveWithTimeout, spawn)
import Pinto.ProcessT.BusT.StateBusT (class UpdateState, Bus, BusMsg(..), BusRef, StateBusT, busRef, create, raise, subscribe, unsubscribe)
import Test.Assert (assertEqual)
import Test.TestHelpers (mpTest)

type TestBus = Bus String TestBusMsg TestBusState
type TestBusRef = BusRef String TestBusMsg TestBusState

data TestBusMsg = TestBusMsg

derive instance Eq TestBusMsg
derive instance Generic TestBusMsg _
instance Show TestBusMsg where
  show = genericShow

data TestBusState = TestBusState Int

derive instance Eq TestBusState
derive instance Generic TestBusState _
instance Show TestBusState where
  show = genericShow

instance UpdateState TestBusState TestBusMsg where
  updateState TestBusMsg (TestBusState i) = TestBusState (i + 1)

data TestMappedMsg
  = TestMappedState Int
  | TestMappedMsg
  | TestMappedTerminated

derive instance Eq TestMappedMsg
derive instance Generic TestMappedMsg _
instance Show TestMappedMsg where
  show = genericShow

data TestAppMsg = TestAppMsg
data TestTimeoutMsg = TestTimeoutMsg

-- foreign import sendSelfLateMessage :: forall name msg. Bus name msg  -> msg -> Milliseconds -> Effect Unit

testStateBusT :: Free TestF Unit
testStateBusT =
  suite "BusM tests" do
    testInitialStateSubscribeThenCreate
    testInitialStateCreateThenSubscribe
    testInitialStateAfterUpdates
    testMapMsg
    testUnsubscribe
    testMultipleBusses

-- TODO testInitialStateCreateTheSubscribe - will fail :)

testInitialStateSubscribeThenCreate :: Free TestF Unit
testInitialStateSubscribeThenCreate =
  mpTest "If you subscribe before a bus is created you get initial state" theTest
  where

  theTest :: StateBusT (BusMsg TestBusMsg TestBusState) (ProcessM Void) Unit
  theTest = do
    subscribe testBusRef identity >>= expect Nothing
    testBus <- createTestBus
    receive >>= expect (Left (State (TestBusState 0)))

    raiseBusMessage testBus
    receive >>= expect (Left (Msg (TestBusMsg)))

testInitialStateCreateThenSubscribe :: Free TestF Unit
testInitialStateCreateThenSubscribe =
  mpTest "If you subscribe after a bus is created you get initial state" theTest
  where

  theTest :: StateBusT (BusMsg TestBusMsg TestBusState) (ProcessM Void) Unit
  theTest = do
    testBus <- createTestBus
    subscribe testBusRef identity >>= (expect (Just (TestBusState 0)))
    raiseBusMessage testBus
    receive >>= expect (Left (Msg (TestBusMsg)))

testInitialStateAfterUpdates :: Free TestF Unit
testInitialStateAfterUpdates =
  mpTest "If you subscribe after messages have been raised, you received the latest state" theTest
  where

  theTest :: StateBusT (BusMsg TestBusMsg TestBusState) (ProcessM Void) Unit
  theTest = do
    testBus <- createTestBus
    raiseBusMessage testBus
    subscribe testBusRef identity >>= (expect (Just (TestBusState 1)))
    raiseBusMessage testBus
    receive >>= expect (Left (Msg (TestBusMsg)))

testMapMsg :: Free TestF Unit
testMapMsg =
  mpTest "We receive mapped messages" theTest
  where

  theTest :: StateBusT TestMappedMsg (ProcessM Void) Unit
  theTest = do
    testBus <- createTestBus
    raiseBusMessage testBus
    subscribe testBusRef mapper >>= (expect (Just (TestBusState 1)))

    raiseBusMessage testBus
    receive >>= expect (Left TestMappedMsg)

  mapper (State (TestBusState i)) = TestMappedState i
  mapper (Msg TestBusMsg) = TestMappedMsg
  mapper BusTerminated = TestMappedTerminated

testUnsubscribe :: Free TestF Unit
testUnsubscribe =
  mpTest "No longer receive messages after unsubscribe" theTest
  where

  theTest :: StateBusT (BusMsg TestBusMsg TestBusState) (ProcessM Void) Unit
  theTest = do
    testBus <- createTestBus
    subscribe testBusRef identity >>= expect (Just (TestBusState 0))
    raiseBusMessage testBus

    receive >>= expect (Left (Msg TestBusMsg))
    unsubscribe testBusRef
    raiseBusMessage testBus
    msg2 <- receiveWithTimeout (Milliseconds 2.0)
    case msg2 of
      Left Timeout ->
        pure unit
      Right _ ->
        unsafeCrashWith "Message after unsubscribe"

{-
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
-}


testMultipleBusses :: Free TestF Unit
testMultipleBusses =
  mpTest "Can subscribe to multiple busses - each with their own mapper" theTest
  where

  theTest :: StateBusT Int (ProcessM Void) Unit
  theTest = do
    testBus <- createTestBus
    subscribe testBusRef (const 1) >>= expect (Just (TestBusState 0))
    let
      mapper = case _ of
        State _ -> 100
        Msg _ -> 10
        BusTerminated -> 1000
    subscribe testBusRef2 mapper >>= expect Nothing
    void $ liftEffect $ spawn $ testBus2Thread \testBus2 ->
      raise testBus2 TestBusMsg
    raiseBusMessage testBus

    int1 <- doReceive
    int2 <- doReceive
    int3 <- doReceive
    -- We should see one Msg from testBus
    -- And one State and one Msg from testBus2
    -- (since it is created after subscription)
    liftEffect $ assertEqual { actual: int1 + int2 + int3, expected: 111 }

  doReceive = do
    msg <- receive
    case msg of
      Left x -> pure x
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
--raiseBusMessage :: ProcessM Void Unit
raiseBusMessage :: forall m. MonadEffect m => TestBus -> m Unit
raiseBusMessage testBus = do
  liftEffect $ raise testBus TestBusMsg

createTestBus :: forall busMsg. StateBusT busMsg (ProcessM Void) TestBus
createTestBus = liftEffect $ create testBusRef (TestBusState 0)

testBus2Thread :: (Bus Atom TestBusMsg TestBusState -> Effect Unit) -> ProcessM Void Unit
testBus2Thread doStuff = liftEffect do
  testBus2 <- create testBusRef2 (TestBusState 0)
  doStuff testBus2

testBusRef :: TestBusRef
testBusRef = busRef "TestBus"

testBusRef2 :: BusRef Atom TestBusMsg TestBusState
testBusRef2 = busRef $ atom "TestBus2"

expect :: forall a m. MonadEffect m => Eq a => Show a => a -> a -> m Unit
expect a b = liftEffect $ expect' a b

expect' :: forall a. Eq a => Show a => a -> a -> Effect Unit
expect' expected actual = assertEqual { actual, expected: expected }
