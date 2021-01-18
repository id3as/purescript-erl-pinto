module Test.DoorLock
       ( startLink
       , State
       , StateId
       , TimerContent
       )
       where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Pinto.GenStatem (class HasStateId, Event(..), InitResult(..), StatemType, Timeout(..), TimeoutAction(..), EventResult(..), CallResult(..), StateEnterResult(..))
import Pinto.GenStatem as Statem
import Pinto.Types (InstanceRef(..), RegistryName(..), ServerPid, crashIfNotStarted)

data StateId
  = StateIdLocked
  | StateIdUnlockedClosed
  | StateIdUnlockedOpen

data State
  = Locked { failedAttempts :: Int }
  | UnlockedClosed { failedAttemptsBeforeUnlock :: Int }
  | UnlockedOpen { failedAttemptsBeforeUnlock :: Int }

type Data =
  { code :: String
  , unknownEvents :: Int
  }

instance stateHasStateId :: HasStateId StateId State where
  getStateId (Locked _) = StateIdLocked
  getStateId (UnlockedClosed _) = StateIdUnlockedClosed
  getStateId (UnlockedOpen _) = StateIdUnlockedOpen

type Info = Void
type Internal = Void
type TimerName = Void
data TimerContent = DoorOpenTooLong

type DoorLockType = StatemType Info Internal TimerName TimerContent Data StateId State

data AuditEvent
  = AuditDoorUnlocked
  | AuditDoorOpened
  | AuditDoorClosed
  | AuditDoorLocked
  | AuditDoorOpenTooLong
  | AuditUnexpectedEventInState


name :: RegistryName DoorLockType
name = Local $ atom "doorLock"


startLink :: Effect (ServerPid DoorLockType)
startLink = do
  crashIfNotStarted <$> (Statem.startLink $ ((Statem.mkSpec init handleEvent) { handleEnter = Just handleEnter }))
  where
    init =
      let
        initialState = Locked { failedAttempts: 0 }
        initialData =
          { code: "1234"
          , unknownEvents: 0
          }
      in do
        _ <- Statem.self
        pure $ InitOk initialState initialData

    handleEnter (Locked _) (UnlockedClosed _) _commonData = do
      _ <- Statem.self
      audit AuditDoorUnlocked # Statem.lift
      pure $ StateEnterKeepData

    handleEnter (UnlockedOpen _) (UnlockedClosed _) _commonData = do
      audit AuditDoorClosed # Statem.lift
      pure $ StateEnterKeepData

    handleEnter _previousState (UnlockedOpen _) _commonData = do
      audit AuditDoorOpened # Statem.lift
      let actions = Statem.newActions # auditIfOpenTooLong
      pure $ StateEnterKeepDataWithActions actions

    handleEnter _previousState (Locked _) _commonData = do
      audit AuditDoorLocked # Statem.lift
      pure $ StateEnterKeepData

    handleEnter _previousState _currentState _commonData = do
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
      Statem.addTimeoutAction (SetStateTimeout (After 300_000 DoorOpenTooLong)) actions

    audit :: AuditEvent -> Effect Unit
    audit _event  = pure unit

data UnlockResult
  = UnlockSuccess
  | InvalidCode
  | InvalidState

unlock :: String -> Effect UnlockResult
unlock code =
  Statem.call (ByName name) impl
  where
    impl from (Locked stateData) commonData@{ code: actualCode } =
      if actualCode == code then do
        let actions = Statem.newActions # Statem.addReply (Statem.mkReply from UnlockSuccess)
        pure $ CallNextStateWithActions (UnlockedClosed { failedAttemptsBeforeUnlock: stateData.failedAttempts }) commonData actions
      else do
        let actions = Statem.newActions # Statem.addReply (Statem.mkReply from InvalidCode)
        pure $ CallNextStateWithActions (Locked (stateData { failedAttempts = stateData.failedAttempts + 1 })) commonData actions

    impl from _invalidState _commonData = do
        let actions = Statem.newActions # Statem.addReply (Statem.mkReply from InvalidState)
        pure $ CallKeepStateAndDataWithActions actions
