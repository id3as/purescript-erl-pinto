module Test.DoorLock
       ( startLink
       , State
       , TimerContent
       )
       where


import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Pinto.GenStatem (CommonAction(..), Event(..), EventAction(..), HandleEventResult(..), Running(..), StateEnterAction(..), StateEnterResult(..), StatemType, Timeout(..), TimeoutAction(..))
import Pinto.GenStatem as Statem
import Pinto.Types (InstanceRef(..), RegistryName(..), ServerPid, StartLinkResult, crashIfNotStarted)
import Unsafe.Coerce (unsafeCoerce)

-- type Cont = Void
-- type Stop = Void
-- data Msg = SetValue Int

data State
  = Locked
  | UnlockedClosed
  | UnlockedOpen

type Data =
  { code :: String
  , attempts :: Int
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
  crashIfNotStarted <$> (Statem.startLink $ Statem.mkSpec init handleEvent)
  where
    init =
      let
        initialState = Locked
        initialData = { code: "1234", attempts: 0 }
      in
        pure $ Right $ Init initialState initialData

    -- handleEnter Locked UnlockedClosed currentData = Statem.lift do
    --   audit AuditDoorUnlocked
    --   pure $ Right $ StateEnterKeepState (currentData { attempts = 0 })

    -- handleEnter UnlockedOpen UnlockedClosed currentData = Statem.lift do
    --   audit AuditDoorClosed
    --   pure $ Right $ StateEnterKeepStateAndData

    -- handleEnter _previousState UnlockedOpen currentData = Statem.lift do
    --   audit AuditDoorOpened
    --   pure $ Right $ StateEnterKeepStateAndDataWithActions (auditIfOpenTooLong : nil)

    -- handleEnter _previousState Locked currentData = Statem.lift do
    --   audit AuditDoorLocked
    --   pure $ Right $ StateEnterKeepStateAndData

    -- handleEnter _previousState _currentState _currentData = Statem.lift do
    --   pure $ Right $ StateEnterKeepStateAndData

    handleEvent (EventStateTimeout DoorOpenTooLong) _state _stateData = Statem.lift do
      audit AuditDoorOpenTooLong
      pure $ HandleEventKeepStateAndData

    handleEvent event _state _stateData = Statem.lift do
      -- TODO: log bad event
      audit AuditUnexpectedEventInState
      pure $ HandleEventKeepStateAndData

    auditIfOpenTooLong = TimeoutAction (SetStateTimeout (After 300_000 DoorOpenTooLong))

    audit :: AuditEvent -> Effect Unit
    audit _event  = pure unit

data UnlockResult
  = UnlockSuccess
  | InvalidCode
  | InvalidState

-- unlock code =
--   Statem.call name impl
--   where
--     impl from Locked stateData@{ code: actualCode } =
--       if actualCode == code then
--         pure $ HandleEventNextStateWithActions UnlockedClosed stateData ((CommonAction $ ReplyAction (Statem.mkReply from UnlockSuccess)) : nil)
--       else
--         pure $ HandleEventKeepStateWithActions ((CommonAction $ ReplyAction (Statem.mkReply from InvalidCode)) : nil) (stateData { attempts = stateData.attempts + 1 })
--
--     impl from _invalidState _data =
--       pure $ HandleEventKeepStateAndDataWithActions ((CommonAction $ ReplyAction (Statem.mkReply from InvalidState)) : nil)
