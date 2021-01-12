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
import Pinto.GenStatem (Event(..), InitResult(..), StatemType, Timeout(..), TimeoutAction(..))
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
      in
        pure $ Init initialState initialData

    handleEnter Locked UnlockedClosed currentData = do
      audit AuditDoorUnlocked # Statem.lift
      Statem.changeStateData (currentData { attempts = 0 })
      pure unit

    handleEnter UnlockedOpen UnlockedClosed currentData = do
      audit AuditDoorClosed # Statem.lift
      pure unit

    handleEnter _previousState UnlockedOpen currentData = do
      audit AuditDoorOpened # Statem.lift
      auditIfOpenTooLong
      pure unit

    handleEnter _previousState Locked currentData = do
      audit AuditDoorLocked # Statem.lift
      pure unit

    handleEnter _previousState _currentState _currentData = do
      pure unit

    handleEvent (EventStateTimeout DoorOpenTooLong) _state _stateData = do
      audit AuditDoorOpenTooLong # Statem.lift
      pure unit

    handleEvent event _state stateData@{ unknownEvents } = do
      -- TODO: log bad event
      audit AuditUnexpectedEventInState # Statem.lift
      Statem.changeStateData (stateData { unknownEvents = unknownEvents + 1 })
      pure unit

    auditIfOpenTooLong = do
      Statem.addTimeoutAction (SetStateTimeout (After 300_000 DoorOpenTooLong))

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
        Statem.addReply (Statem.mkReply from UnlockSuccess)
        Statem.changeState UnlockedClosed
        pure unit
      else do
        Statem.addReply (Statem.mkReply from InvalidCode)
        Statem.changeStateData (stateData { attempts = stateData.attempts + 1 })
        pure unit

    impl from _invalidState _data = do
      Statem.addReply (Statem.mkReply from InvalidState)
      pure unit
