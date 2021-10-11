module Test.DoorLock
  ( testSuite
  , startLink
  , State
  , StateId
  , TimerContent
  , DoorLockPid
  ) where

import Prelude
import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Process.Raw (class HasPid)
import Erl.Test.EUnit (TestF, suite, test)
import Pinto.GenStatem (class HasStateId, Event(..), InitResult(..), StatemPid, StatemType, Timeout(..), TimeoutAction(..), EventResult(..), StateEnterResult(..), StatemRef(..))
import Pinto.GenStatem as Statem
import Pinto.Types (RegistryName(..), RegistryReference(..), crashIfNotStarted)
import Test.Assert (assertEqual)

-- -----------------------------------------------------------------------------
-- Test Implementation
-- -----------------------------------------------------------------------------
testSuite :: Free TestF Unit
testSuite =
  suite "Pinto 'DoorLock' GenStatem Tests" do
    test "Can create a DoorLock and interact with it" do
      serverPid <- startLink
      failedOpenResultLocked <- open
      assertEqual { actual: failedOpenResultLocked, expected: OpenFailedInvalidState }
      failedUnlockResult <- unlock "NOT_THE_CODE"
      assertEqual { actual: failedUnlockResult, expected: InvalidCode }
      successUnlockResult <- unlock "THE_CODE"
      assertEqual { actual: successUnlockResult, expected: UnlockSuccess }
      invalidStateUnlockResult <- unlock "NO_LONGER_MATTERS"
      assertEqual { actual: invalidStateUnlockResult, expected: InvalidState }
      successOpenResult <- open
      assertEqual { actual: successOpenResult, expected: OpenSuccess }
      failedOpenResultAlreadyOpen <- open
      assertEqual { actual: failedOpenResultAlreadyOpen, expected: OpenFailedInvalidState }
      -- to test
      -- - cast
      -- - timeout (door open too long)
      -- - info messages
      -- - internal messages
      -- - regular timeout messages
      -- - at timeouts?
      -- - timeout cancelation
      -- - stop
      -- - ignore
      -- to implement
      -- - named timeouts
      -- - postpone (incl not_implemented)
      -- - next event
      -- - hibernate
      -- let
      --   instanceRef = ByPid serverPid
      -- state1 <- getState instanceRef
      -- state2 <- setState instanceRef (TestState 1)
      -- state3 <- getState instanceRef
      -- setStateCast instanceRef (TestState 2)
      -- state4 <- getState instanceRef
      -- assertEqual { actual: state1, expected: TestState 0 }
      -- assertEqual { actual: state2, expected: TestState 0 }
      -- assertEqual { actual: state3, expected: TestState 1 }
      -- assertEqual { actual: state4, expected: TestState 2 }
      pure unit

-- -----------------------------------------------------------------------------
-- Statem Implementation
-- -----------------------------------------------------------------------------
data StateId
  = StateIdLocked
  | StateIdUnlockedClosed
  | StateIdUnlockedOpen

derive instance eqStateId :: Eq StateId

data State
  = Locked { failedAttempts :: Int }
  | UnlockedClosed { failedAttemptsBeforeUnlock :: Int }
  | UnlockedOpen { failedAttemptsBeforeUnlock :: Int }

instance stateHasStateId :: HasStateId StateId State where
  getStateId (Locked _) = StateIdLocked
  getStateId (UnlockedClosed _) = StateIdUnlockedClosed
  getStateId (UnlockedOpen _) = StateIdUnlockedOpen

type Data
  = { code :: String
    , unknownEvents :: Int
    }

type Info
  = Void

type Internal
  = Void

type TimerName
  = Void

data TimerContent
  = DoorOpenTooLong

type DoorLockType
  = StatemType Info Internal TimerName TimerContent Data StateId State

newtype DoorLockPid
  = DoorLockPid (StatemPid Info Internal TimerName TimerContent Data StateId State)

-- Only surface the raw pid, don't implement HasProcess - we don't want folks sending us messages using our Info
-- type
derive newtype instance doorLockPidHasPid :: HasPid DoorLockPid

data AuditEvent
  = AuditDoorUnlocked
  | AuditDoorOpened
  | AuditDoorClosed
  | AuditDoorLocked
  | AuditDoorOpenTooLong
  | AuditUnexpectedEventInState

name :: RegistryName DoorLockType
name = Local $ atom "doorLock"

startLink :: Effect DoorLockPid
startLink = do
  DoorLockPid <$> crashIfNotStarted <$> (Statem.startLink $ ((Statem.defaultSpec init handleEvent) { name = Just name, handleEnter = Just handleEnter }))
  where
  init =
    let
      initialState = Locked { failedAttempts: 0 }

      initialData =
        { code: "THE_CODE"
        , unknownEvents: 0
        }
    in
      do
        _ <- Statem.self
        pure $ InitOk initialState initialData

  handleEnter StateIdLocked StateIdUnlockedClosed _state _commonData = do
    _ <- Statem.self
    audit AuditDoorUnlocked # Statem.lift
    pure $ StateEnterKeepData

  handleEnter StateIdUnlockedOpen StateIdUnlockedClosed _state _commonData = do
    audit AuditDoorClosed # Statem.lift
    pure $ StateEnterKeepData

  handleEnter _previousStateId StateIdUnlockedOpen _state _commonData = do
    audit AuditDoorOpened # Statem.lift
    let
      actions = Statem.newActions # auditIfOpenTooLong
    pure $ StateEnterKeepDataWithActions actions

  handleEnter _previousStateId StateIdLocked _state _commonData = do
    audit AuditDoorLocked # Statem.lift
    pure $ StateEnterKeepData

  handleEnter _previousStateId _currentStateId _state _commonData = do
    pure $ StateEnterKeepData

  handleEvent (EventStateTimeout DoorOpenTooLong) _state _commonData = do
    audit AuditDoorOpenTooLong # Statem.lift
    pure $ EventKeepStateAndData

  handleEvent event state commonData@{ unknownEvents } = do
    -- TODO: log bad event
    _ <- Statem.self
    audit AuditUnexpectedEventInState # Statem.lift
    pure $ EventKeepState (commonData { unknownEvents = unknownEvents + 1 })

  auditIfOpenTooLong actions = do
    Statem.addTimeoutAction (SetStateTimeout (After 0 DoorOpenTooLong)) actions

  audit :: AuditEvent -> Effect Unit
  audit event = do
    pure unit

-- -----------------------------------------------------------------------------
-- Door Unlock
-- -----------------------------------------------------------------------------
data UnlockResult
  = UnlockSuccess
  | InvalidCode
  | InvalidState

derive instance eqUnlockResult :: Eq UnlockResult

instance showUnlockResult :: Show UnlockResult where
  show UnlockSuccess = "Success"
  show InvalidCode = "Invalid Code"
  show InvalidState = "Invalid State"

unlock :: String -> Effect UnlockResult
unlock code = Statem.call (ByName name) impl
  where
  impl from (Locked stateData) commonData@{ code: actualCode } =
    if actualCode == code then do
      let
        actions = Statem.newActions # Statem.addReply (Statem.reply from UnlockSuccess)
      pure $ EventNextStateWithActions (UnlockedClosed { failedAttemptsBeforeUnlock: stateData.failedAttempts }) commonData actions
    else do
      let
        actions = Statem.newActions # Statem.addReply (Statem.reply from InvalidCode)
      pure $ EventNextStateWithActions (Locked (stateData { failedAttempts = stateData.failedAttempts + 1 })) commonData actions

  impl from _invalidState _commonData = do
    let
      actions = Statem.newActions # Statem.addReply (Statem.reply from InvalidState)
    pure $ EventKeepStateAndDataWithActions actions

-- -----------------------------------------------------------------------------
-- Door Open
-- -----------------------------------------------------------------------------
data OpenResult
  = OpenSuccess
  | OpenFailedInvalidState

derive instance eqOpenResult :: Eq OpenResult

instance showOpenResult :: Show OpenResult where
  show OpenSuccess = "Success"
  show OpenFailedInvalidState = "Invalid State"

open :: Effect OpenResult
open = Statem.call (ByName name) impl
  where
  impl from (UnlockedClosed { failedAttemptsBeforeUnlock }) commonData =
    let
      actions = Statem.newActions # Statem.addReply (Statem.reply from OpenSuccess)
    in
      pure $ EventNextStateWithActions (UnlockedOpen { failedAttemptsBeforeUnlock }) commonData actions

  impl from _invalidState _commonData =
    let
      actions = Statem.newActions # Statem.addReply (Statem.reply from OpenFailedInvalidState)
    in
      pure $ EventKeepStateAndDataWithActions actions
