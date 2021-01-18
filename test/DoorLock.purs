module Test.DoorLock
       ( startLink
       , State
       , TimerContent
       )
       where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Pinto.GenStatem (Event(..), InitResult(..), StatemType, Timeout(..), TimeoutAction(..), EventResult(..), CallResult(..), StateEnterResult(..))
import Pinto.GenStatem as Statem
import Pinto.Types (InstanceRef(..), RegistryName(..), ServerPid, crashIfNotStarted)

data State
  = Locked
  | UnlockedClosed
  | UnlockedOpen

type Data =
  { code :: String
  , attempts :: Int
  , unknownEvents :: Int
  }

type Info = Void
type Internal = Void
type TimerName = Void
data TimerContent
  = DoorOpenTooLong

type DoorLockType = StatemType Info Internal TimerName TimerContent State Data

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
        initialState = Locked
        initialData =
          { code: "1234"
          , attempts: 0
          , unknownEvents: 0
          }
      in do
        _ <- Statem.self
        pure $ InitOk initialState initialData

    handleEnter Locked UnlockedClosed currentData = do
      _ <- Statem.self
      audit AuditDoorUnlocked # Statem.lift
      pure $ StateEnterKeepState (currentData { attempts = 0 })

    handleEnter UnlockedOpen UnlockedClosed currentData = do
      audit AuditDoorClosed # Statem.lift
      pure $ StateEnterKeepStateAndData

    handleEnter _previousState UnlockedOpen currentData = do
      audit AuditDoorOpened # Statem.lift
      let actions = Statem.newActions # auditIfOpenTooLong
      pure $ StateEnterKeepStateAndDataWithActions actions

    handleEnter _previousState Locked currentData = do
      audit AuditDoorLocked # Statem.lift
      pure $ StateEnterKeepStateAndData

    handleEnter _previousState _currentState _currentData = do
      pure $ StateEnterKeepStateAndData

    handleEvent (EventStateTimeout DoorOpenTooLong) _state _stateData = do
      audit AuditDoorOpenTooLong # Statem.lift
      pure $ EventKeepStateAndData

    handleEvent event _state stateData@{ unknownEvents } = do
      -- TODO: log bad event
      _ <- Statem.self
      audit AuditUnexpectedEventInState # Statem.lift
      pure $ EventKeepState (stateData { unknownEvents = unknownEvents + 1 })

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
    impl from Locked stateData@{ code: actualCode } =
      if actualCode == code then do
        let actions = Statem.newActions # Statem.addReply (Statem.mkReply from UnlockSuccess)
        pure $ CallNextStateWithActions UnlockedClosed stateData actions
      else do
        let actions = Statem.newActions # Statem.addReply (Statem.mkReply from InvalidCode)
        pure $ CallKeepStateWithActions (stateData { attempts = stateData.attempts + 1 }) actions

    impl from _invalidState _data = do
        let actions = Statem.newActions # Statem.addReply (Statem.mkReply from InvalidState)
        pure $ CallKeepStateAndDataWithActions actions
