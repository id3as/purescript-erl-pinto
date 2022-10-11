module Test.StateMetadataBusT
  ( testStateMetadataBusT
  ) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Erl.Atom (Atom, atom)
import Erl.Process (Process, self, (!))
import Erl.Test.EUnit (TestF, suite)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT (class MonadProcessHandled, class MonadProcessTrans, ProcessTM, ProcessM, receive, receiveWithTimeout, spawn)
import Pinto.ProcessT.BusT.MetadataBusT (MetadataBusT)
import Pinto.ProcessT.BusT.MetadataBusT as M
import Pinto.ProcessT.BusT.StateBusT (class UpdateState, StateBusT)
import Pinto.ProcessT.BusT.StateBusT as S
import Test.Assert (assertEqual)
import Test.TestHelpers (mpTest)

type TestBusM = M.Bus String TestBusMsg TestBusMetadata
type TestBusRefM = M.BusRef String TestBusMsg TestBusMetadata

type TestBusS = S.Bus String TestBusMsg TestBusState
type TestBusRefS = S.BusRef String TestBusMsg TestBusState

type StackM = MetadataBusT TestMappedMsgM
type StackS = StateBusT TestMappedMsgS

type StackMS = StackM (StackS (ProcessTM Ack (Either TestMappedMsgM (Either TestMappedMsgS Ack)))) Unit

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

data TestBusMetadata = TestBusMetadata String

derive instance Eq TestBusMetadata
derive instance Generic TestBusMetadata _
instance Show TestBusMetadata where
  show = genericShow

data TestMappedMsgM
  = TestMappedMetadata String String
  | TestMappedMsgM String
  | TestMappedTerminatedM String

data TestMappedMsgS
  = TestMappedState String Int
  | TestMappedMsgS String
  | TestMappedTerminatedS String

mapperM :: String -> M.BusMsg TestBusMsg TestBusMetadata -> TestMappedMsgM
mapperM whence (M.MetadataMsg (TestBusMetadata i)) = TestMappedMetadata whence i
mapperM whence (M.DataMsg TestBusMsg) = TestMappedMsgM whence
mapperM whence M.BusTerminated = TestMappedTerminatedM whence

mapperS :: String -> S.BusMsg TestBusMsg TestBusState -> TestMappedMsgS
mapperS whence (S.State (TestBusState i)) = TestMappedState whence i
mapperS whence (S.Msg TestBusMsg) = TestMappedMsgS whence
mapperS whence S.BusTerminated = TestMappedTerminatedS whence

derive instance Eq TestMappedMsgM
derive instance Generic TestMappedMsgM _
instance Show TestMappedMsgM where
  show = genericShow

derive instance Eq TestMappedMsgS
derive instance Generic TestMappedMsgS _
instance Show TestMappedMsgS where
  show = genericShow

data TestAppMsg = TestAppMsg
data TestTimeoutMsg = TestTimeoutMsg

-- TODO: actually test sending metadata with updateMetadata :)

testStateMetadataBusT :: Free TestF Unit
testStateMetadataBusT =
  suite "StateMetadataBusT tests" do
    mainTest

expectMessage :: String -> StackMS
expectMessage whence = receive >>= expect (Right (Left (TestMappedMsgS whence)))

expectState :: String -> Int -> StackMS
expectState whence s = receive >>= expect (Right (Left (TestMappedState whence s)))

expectTerminated :: String -> StackMS
expectTerminated whence = receive >>= expect (Left (TestMappedTerminatedM whence))

expectData :: String -> StackMS
expectData whence = receive >>= expect (Left (TestMappedMsgM whence))

expectMetadata :: String -> String -> StackMS
expectMetadata whence m = receive >>= expect (Left (TestMappedMetadata whence m))

mainTest :: Free TestF Unit
mainTest = do
  mpTest "comprehensive test for a stack of MetadataBus and StateBus" theTest
  where

  -- Creates two metadata busses, one state bus all on the same transformer stack
  -- (one metadata bus on a new thread)
  theTest :: StackMS
  theTest = do
    me <- self

    testBusM <- createTestBusM
    M.subscribe testBusRefM (mapperM "metadata bus 1") >>= expect (Just (TestBusMetadata "initial metadata"))

    raiseBusData testBusM <* expectData "metadata bus 1"

    (lift $ S.subscribe testBusRefS (mapperS "state bus 1")) >>= expect Nothing
    testBusS <- createTestBusS <* expectState "state bus 1" 0

    raiseBusMessage testBusS <* expectMessage "state bus 1"

    raiseBusMetadata testBusM (TestBusMetadata "second metadata") <* expectMetadata "metadata bus 1" "second metadata"

    raiseBusData testBusM <* expectData "metadata bus 1"

    senderPid <- liftEffect $ spawn (testBusThreadHelperM (Just Ack) me testBusRef2M)
    liftEffect $ senderPid ! CreateBus (TestBusMetadata "initial")
    receive >>= case _ of
      Right (Right Ack) -> pure unit
      _ -> unsafeCrashWith "Expected Ack"
    M.subscribe testBusRef2M (mapperM "metadata bus 2") >>= expect (Just (TestBusMetadata "initial"))

    raiseBusMessage testBusS <* expectMessage "state bus 1"

    raiseBusMetadata testBusM (TestBusMetadata "third metadata") <* expectMetadata "metadata bus 1" "third metadata"

    liftEffect $ senderPid ! RaiseMessage TestBusMsg
    expectData "metadata bus 2"

    -- note: important to end the spawned thread, otherwise it still owns the gproc name and leads to
    -- type errors in MetadataBusT tests since they share bus names but Metadata is a string here and
    -- an integer there:
    -- > *failed* in function erlang:integer_to_binary/1, called as integer_to_binary(<<"initial">>)
    liftEffect $ senderPid ! DeleteAndExit
    expectTerminated "metadata bus 2"

    noMoreMessages

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

raiseBusMessage :: forall m. MonadEffect m => TestBusS -> m Unit
raiseBusMessage testBus = do
  liftEffect $ S.raise testBus TestBusMsg

raiseBusData :: forall m. MonadEffect m => TestBusM -> m Unit
raiseBusData testBus = do
  liftEffect $ M.raise testBus TestBusMsg

raiseBusMetadata :: forall m. MonadEffect m => TestBusM -> TestBusMetadata -> m Unit
raiseBusMetadata testBus m = do
  liftEffect $ M.updateMetadata testBus m

createTestBusM :: forall m. MonadEffect m => m TestBusM
createTestBusM = liftEffect $ M.create testBusRefM (TestBusMetadata "initial metadata")

createTestBusS :: forall m. MonadEffect m => m TestBusS
createTestBusS = liftEffect $ S.create testBusRefS (TestBusState 0)

data Ack = Ack

derive instance Eq Ack
instance Show Ack where
  show Ack = "Ack"

data HelperMsg
  = CreateBus TestBusMetadata
  | RaiseMessage TestBusMsg
  | DeleteAndWait
  | DeleteAndExit
  | ExitNormal
  | ExitCrash

derive instance Eq HelperMsg

testBusThreadHelperM :: forall ack. Maybe ack -> Process ack -> M.BusRef Atom TestBusMsg TestBusMetadata -> ProcessM HelperMsg Unit
testBusThreadHelperM ack parent localBusRef = do
  localBus <- receive >>= case _ of
    CreateBus initialMetadata -> do
      localBus <- liftEffect $ M.create localBusRef initialMetadata
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
        liftEffect $ M.raise localBus msg
        testBusThreadLoop localBus
      DeleteAndExit -> do
        liftEffect $ M.delete localBus
      DeleteAndWait -> do
        liftEffect $ M.delete localBus
        testBusThreadLoop localBus
      ExitNormal -> do
        pure unit
      ExitCrash -> do
        unsafeCrashWith "Asked to crash"

testBusRefM :: TestBusRefM
testBusRefM = M.busRef "TestBus"

testBusRef2M :: M.BusRef Atom TestBusMsg TestBusMetadata
testBusRef2M = M.busRef $ atom "TestBus2"

testBusRefS :: TestBusRefS
testBusRefS = S.busRef "TestBus"

testBusRef2S :: S.BusRef Atom TestBusMsg TestBusState
testBusRef2S = S.busRef $ atom "TestBus2"

expect :: forall a m. MonadEffect m => Eq a => Show a => a -> a -> m Unit
expect a b = liftEffect $ expect' a b

expect' :: forall a. Eq a => Show a => a -> a -> Effect Unit
expect' expected actual = assertEqual { actual, expected: expected }

noMoreMessages :: forall m mState appMsg parsedMsg. MonadProcessHandled m parsedMsg => MonadProcessTrans m mState appMsg parsedMsg => MonadEffect m => m Unit
noMoreMessages = receiveWithTimeout (Milliseconds 6.0) >>= case _ of
  Left _ -> pure unit
  Right _ -> unsafeCrashWith "Received unexpected message"
