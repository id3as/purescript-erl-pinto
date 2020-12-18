module Test.DoorLock
       ( startLink
       , State
       )
       where


import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Pinto.GenStatem (StateEnterAction(..), StateEnterResult(..), StatemType, Timeout(..), TimeoutAction(..), Running(..))
import Pinto.GenStatem as Statem
import Pinto.Types (InstanceRef(..), RegistryName(..), ServerPid, StartLinkResult, crashIfNotStarted)
import Unsafe.Coerce (unsafeCoerce)

-- type Cont = Void
-- type Stop = Void
-- data Msg = SetValue Int

data EventContent
  = DoorOpenTooLong

data State
  = Locked
  | UnlockedClosed
  | UnlockedOpen

type Data = {}

type DoorLockType = StatemType State Data

data AuditEvent
  = AuditDoorUnlocked
  | AuditDoorOpened
  | AuditDoorClosed
  | AuditDoorLocked


name :: RegistryName DoorLockType
name = Local $ atom "doorLock"


startLink :: Effect (ServerPid DoorLockType)
startLink = do
  crashIfNotStarted <$> statem
  where
    spec :: Statem.Spec State Data
    spec = Statem.mkSpec

    statem :: Effect (StartLinkResult DoorLockType)
    statem = Statem.startLink spec
    -- init =
    --   let
    --     initialState = Locked
    --     initialData = { }
    --   in
    --     pure $ Right $ InitOk initialState initialData

    handleEvent = unsafeCoerce unit

    handleEnter Locked UnlockedClosed currentData = do
      audit AuditDoorUnlocked
      pure $ Right $ KeepStateAndData

    handleEnter UnlockedOpen UnlockedClosed currentData = do
      audit AuditDoorClosed
      pure $ Right $ KeepStateAndData

    handleEnter _previousState UnlockedOpen currentData = do
      audit AuditDoorOpened
      pure $ Right $ KeepStateAndDataWithActions (auditIfOpenTooLong : nil)

    handleEnter _previousState Locked currentData = do
      audit AuditDoorLocked
      pure $ Right $ KeepStateAndData

    handleEnter _previousState _currentState _currentData = do
      pure $ Right $ KeepStateAndData

    auditIfOpenTooLong = TimeoutAction (SetStateTimeout (After 300_000 (Just DoorOpenTooLong)))

    audit :: AuditEvent -> Effect Unit
    audit _event  = pure unit
