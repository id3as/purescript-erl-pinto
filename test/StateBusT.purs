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
import Effect.Class.Console (log)
import Erl.Atom (Atom, atom)
import Erl.Kernel.Erlang (sleep)
import Erl.Process (Process, self, (!))
import Erl.Test.EUnit (TestF, suite)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (ProcessTM, Timeout(..), ProcessM, receive, receiveWithTimeout, spawn)
import Pinto.ProcessT.BusT.StateBusT (class UpdateState, Bus, BusMsg(..), BusRef, StateBusT, busRef, create, delete, raise, subscribe, unsubscribe)
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

testStateBusT :: Free TestF Unit
testStateBusT =
  suite "StateBusT tests" do
    testInitialStateSubscribeThenCreate
    testInitialStateCreateThenSubscribe
    testInitialStateAfterUpdates
    testMapMsg
    testUnsubscribe
    testMultipleBusses
    testSenderExits
    testErrorHandling

testInitialStateSubscribeThenCreate :: Free TestF Unit
testInitialStateSubscribeThenCreate =
  mpTest "If you subscribe before a bus is created you get initial state" theTest
  where

  theTest :: StateBusT (BusMsg TestBusMsg TestBusState) (ProcessTM Void _) Unit
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

  theTest :: StateBusT (BusMsg TestBusMsg TestBusState) (ProcessTM Void _) Unit
  theTest = do
    testBus <- createTestBus
    subscribe testBusRef identity >>= (expect (Just (TestBusState 0)))
    raiseBusMessage testBus
    receive >>= expect (Left (Msg (TestBusMsg)))

testInitialStateAfterUpdates :: Free TestF Unit
testInitialStateAfterUpdates =
  mpTest "If you subscribe after messages have been raised, you received the latest state" theTest
  where

  theTest :: StateBusT (BusMsg TestBusMsg TestBusState) (ProcessTM Void _) Unit
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

  theTest :: StateBusT TestMappedMsg (ProcessTM Void _) Unit
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

  theTest :: StateBusT (BusMsg TestBusMsg TestBusState) (ProcessTM Void _) Unit
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

testSenderExits :: Free TestF Unit
testSenderExits = do
  mpTest "Receive BusTerminated if sender exits (create then subscribe, normal exit)" (theTest ExitNormal)
  mpTest "Receive BusTerminated if sender exits (create then subscribe, crash exit)" (theTest ExitCrash)
  mpTest "Receive BusTerminated if sender exits (subscribe then create, normal exit)" (theTest2 ExitNormal)
  mpTest "Receive BusTerminated if sender exits (subscribe then create, deletion)" (theTest2 DeleteAndWait)
  where

  theTest :: _ -> StateBusT (BusMsg TestBusMsg TestBusState) (ProcessTM Ack _) Unit
  theTest howToExit = do
    -- Create a StateBus
    -- Register for it
    -- The StateBus sneder thread goes away (exits normally, crashes, etc.)
    let ourTestBus = busRef $ atom "TestingBus"
    me <- self
    senderPid <- liftEffect $ spawn (testBusThreadHelper (Just Ack) me ourTestBus)

    liftEffect $ senderPid ! CreateBus (TestBusState 0)
    receive >>= case _ of
      Right Ack -> pure unit
      Left _ -> unsafeCrashWith "Expected Ack"
    subscribe ourTestBus identity >>= expect (Just (TestBusState 0))

    liftEffect $ senderPid ! RaiseMessage TestBusMsg
    receive >>= expect (Left (Msg TestBusMsg))

    liftEffect $ senderPid ! howToExit
    -- Milliseconds 2.0 is too short for crashing case on my machine
    receiveWithTimeout (Milliseconds 6.0) >>= case _ of
      Left Timeout ->
        unsafeCrashWith "Did not receive BusTerminated before timeout"
      Right (Right Ack) ->
        unsafeCrashWith "Did not want acknowledgement at this time :("
      Right (Left BusTerminated) ->
        pure unit
      Right (Left _) ->
        unsafeCrashWith "Received unexpected message (not BusTerminated)"
    when (howToExit == DeleteAndWait) do
      liftEffect $ senderPid ! ExitNormal

  theTest2 :: _ -> StateBusT (BusMsg TestBusMsg TestBusState) (ProcessTM Void _) Unit
  theTest2 howToExit = do
    let ourTestBus = busRef $ atom "TestingBus"
    me <- self
    -- Subscribe before creation
    subscribe ourTestBus identity >>= expect Nothing
    senderPid <- liftEffect $ spawn (testBusThreadHelper Nothing me ourTestBus)

    liftEffect $ senderPid ! CreateBus (TestBusState 0)
    receive >>= expect (Left (State (TestBusState 0)))

    liftEffect $ senderPid ! RaiseMessage TestBusMsg
    receive >>= expect (Left (Msg TestBusMsg))

    liftEffect $ senderPid ! howToExit
    receiveWithTimeout (Milliseconds 6.0) >>= case _ of
      Left Timeout ->
        unsafeCrashWith "Did not receive BusTerminated before timeout"
      Right (Left BusTerminated) ->
        pure unit
      Right _ ->
        unsafeCrashWith "Received unexpected message (not BusTerminated)"
    when (howToExit == DeleteAndWait) do
      liftEffect $ senderPid ! ExitNormal

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

  theTest :: StateBusT Int (ProcessTM Void _) Unit
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
    int4 <- doReceive
    -- We should see one Msg from testBus
    -- And one State and one Msg from testBus2
    -- (since it is created after subscription)
    liftEffect $ assertEqual { actual: int1 + int2 + int3 + int4, expected: 1111 }

  doReceive = do
    msg <- receive
    case msg of
      Left x -> pure x
      Right _ ->
        unsafeCrashWith "We got sent a void message!"

-- Test that a particular error can happen in subscribe
-- Bad code to trigger `error:badarg` from `gproc:get_attribute`:
--
-- ```
-- true = gproc:reg(?gprocPropertyKey(BusName)),
-- Pid = gproc:where(?gprocNameKey(BusName)),
-- case Pid of
--   undefined -> ?nothing;
--   _ ->
--     Ref = (monitorImpl(Pid, BusName))(),
--     timer:sleep(10),
--     {Gen, State} = gproc:get_attribute(?gprocNameKey(BusName), Pid, ?stateTag),
--     ?just({Gen, State, Ref})
-- end
-- ```
testErrorHandling :: Free TestF Unit
testErrorHandling =
  mpTest "Handles unregistration-during-subscribe gracefully" theTest
  where

  theTest :: StateBusT _ (ProcessTM Unit _) Unit
  theTest = do
    me <- self
    _ <- liftEffect $ spawn $ testBus2Thread \testBus -> do
      me ! unit
      -- wait to hit internal `timer:sleep(10)` in subscribe
      sleep (Milliseconds 5.0)
      delete testBus
    -- wait for bus creation
    receive >>= expect (Right unit)
    -- We receive `Just (TestBusState 0)` if subscribe is quick
    -- We would receive `Nothing` if subscribe is delayed
    -- But without a try/catch around `gproc:getattribute`, it would throw `error:badarg`
    subscribe testBusRef2 identity >>= expect (Just (TestBusState 0))

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
--raiseBusMessage :: ProcessM Void Unit
raiseBusMessage :: forall m. MonadEffect m => TestBus -> m Unit
raiseBusMessage testBus = do
  liftEffect $ raise testBus TestBusMsg

createTestBus :: forall handledMsg busMsg. StateBusT busMsg (ProcessTM Void handledMsg) TestBus
createTestBus = liftEffect $ create testBusRef (TestBusState 0)

testBus2Thread :: (Bus Atom TestBusMsg TestBusState -> Effect Unit) -> ProcessM Void Unit
testBus2Thread doStuff = liftEffect do
  testBus2 <- create testBusRef2 (TestBusState 0)
  doStuff testBus2

data Ack = Ack

derive instance Eq Ack
instance Show Ack where
  show Ack = "Ack"

data HelperMsg
  = CreateBus TestBusState
  | RaiseMessage TestBusMsg
  | DeleteAndWait
  | DeleteAndExit
  | ExitNormal
  | ExitCrash

derive instance Eq HelperMsg

testBusThreadHelper :: forall ack. Maybe ack -> Process ack -> BusRef Atom TestBusMsg TestBusState -> ProcessM HelperMsg Unit
testBusThreadHelper ack parent localBusRef = do
  localBus <- receive >>= case _ of
    CreateBus initialState -> do
      localBus <- liftEffect $ create localBusRef initialState
      case ack of
        Just msg -> liftEffect $ parent ! msg
        Nothing -> pure unit
      pure localBus
    _ -> do
      log "Unexpected helper message"
      unsafeCrashWith "Unexpected helper message"
  testBusThreadLoop localBus
  where
  testBusThreadLoop localBus =
    receive >>= case _ of
      CreateBus _ -> do
        log "Cannot create twice!"
        unsafeCrashWith "Cannot create twice!"
      RaiseMessage msg -> do
        liftEffect $ raise localBus msg
        testBusThreadLoop localBus
      DeleteAndExit -> do
        liftEffect $ delete localBus
      DeleteAndWait -> do
        liftEffect $ delete localBus
        testBusThreadLoop localBus
      ExitNormal -> do
        pure unit
      ExitCrash -> do
        unsafeCrashWith "Asked to crash"

testBusRef :: TestBusRef
testBusRef = busRef "TestBus"

testBusRef2 :: BusRef Atom TestBusMsg TestBusState
testBusRef2 = busRef $ atom "TestBus2"

expect :: forall a m. MonadEffect m => Eq a => Show a => a -> a -> m Unit
expect a b = liftEffect $ expect' a b

expect' :: forall a. Eq a => Show a => a -> a -> Effect Unit
expect' expected actual = assertEqual { actual, expected: expected }
