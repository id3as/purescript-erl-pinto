-- | This module represents gen_statem in OTP
-- | See also gen_statem in the OTP docs (https://erlang.org/doc/man/gen_statem.html)
-- | Note: This module is largely undocumented and is in alpha at the current time
-- | it works largely like `Pinto.GenServer`, but with even more types
module Pinto.GenStatem
  ( StatemType
  , StatemPid
  , StatemRef(..)
  , class HasStateId
  , getStateId
  , class SupportsReply
  , addReply
  , class SupportsAddTimeout
  , addTimeoutAction
  , class SupportsNextEvent
  , addNextEvent
  , class SupportsSelf
  , self
  , class SupportsNewActions
  , newActions
  , DownReason
  , InitFn
  , InitResult(..)
  , InitT
  , InitActionsBuilder(..)
  , EventAction(..)
  , CommonAction(..)
  , CastFn
  , CallFn
  , HandleEventFn
  , EventFn
  , EventResult(..)
  , TerminateFn
  , EventT
  , EventActionsBuilder
  , EnterFn
  , StateEnterResult(..)
  , StateEnterT
  , StateEnterActionsBuilder
  , TimeoutAction(..)
  , NamedTimeoutAction(..)
  , Timeout(..)
  , Event(..)
  , Spec
  , From
  , Reply
  , reply
  , startLink
  , procLibStartLink
  , defaultSpec
  , call
  , cast
  , module Exports
  , module TypeExports
  -- As before, these could be adifferent module
  , init
  , callback_mode
  , handle_event
  , terminate
  , NativeInitResult
  , NativeAction
  , OuterData
  , NativeHandleEventResult
  ) where

import Prelude
import Control.Monad.State.Trans (StateT)
import Control.Monad.State.Trans as StateT
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Trans.Class (lift) as Exports
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, EffectFn4, mkEffectFn1, mkEffectFn3, mkEffectFn4)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, (:), nil)
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Erl.ModuleName (NativeModuleName, nativeModuleName)
import Erl.Process (Process, class HasProcess)
import Erl.Process.Raw (class HasPid, setProcessFlagTrapExit)
import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ModuleNames (pintoGenStatem)
import Pinto.Types (ShutdownReason(..), ExitMessage(..)) as TypeExports
import Pinto.Types (class ExportsTo, ExitMessage(..), RegistryInstance, RegistryName, RegistryReference, ShutdownReason, StartLinkResult, export, parseShutdownReasonFFI, parseTrappedExitFFI, registryInstance)
import Unsafe.Coerce (unsafeCoerce)

-- -----------------------------------------------------------------------------
-- States
-- -----------------------------------------------------------------------------
class HasStateId stateId state where
  getStateId :: state -> stateId

-- -----------------------------------------------------------------------------
-- Response Type Classes
-- -----------------------------------------------------------------------------
class SupportsReply builder where
  addReply :: Reply -> builder -> builder

class SupportsAddTimeout builder timerContent where
  addTimeoutAction :: TimeoutAction timerContent -> builder timerContent -> builder timerContent

class SupportsNextEvent builder event | builder -> event where
  addNextEvent :: event -> builder -> builder

class SupportsSelf :: (Type -> Type -> Type -> Type -> Type -> Type -> Type -> (Type -> Type) -> Type -> Type) -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Constraint
class SupportsSelf context info internal timerName timerContent commonData stateId state where
  self :: context info internal timerName timerContent commonData stateId state Effect (StatemPid info internal timerName timerContent commonData stateId state)

data DownReason
  = DownNormal
  | DownNoConnection
  | DownOther Foreign

class SupportsNewActions builder where
  newActions :: builder

type InitContext :: forall k1 k2 k3 k4 k5 k6 k7. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> Type
type InitContext info internal timerName timerContent commonData stateId state
  = Context info internal timerName timerContent commonData stateId state

newtype InitActionsBuilder info internal timerName timerContent
  = InitActionsBuilder (List (EventAction info internal timerName timerContent))

newtype InitT :: forall k1 k2 k3 k4 k5 k6 k7. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> (Type -> Type) -> Type -> Type
newtype InitT info internal timerName timerContent commonData stateId state m a
  = InitT (StateT (InitContext info internal timerName timerContent commonData stateId state) m a)

derive newtype instance functorInit :: Functor (InitT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance applyInit :: Apply (InitT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance applicativeInit :: Applicative (InitT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance bindInit :: Bind (InitT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance monadInit :: Monad (InitT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance monadTransInit :: MonadTrans (InitT info internal timerName timerContent commonData stateId state)

instance supportsSelfInitT :: SupportsSelf InitT info internal timerName timerContent commonData stateId state where
  self = InitT $ Exports.lift $ selfFFI

instance supportsNewActionsInitActionsBuilder :: SupportsNewActions (InitActionsBuilder info internal timerName timerContent) where
  newActions = InitActionsBuilder nil

instance supportsReplyInitActionsBuilder :: SupportsReply (InitActionsBuilder info internal timerName timerContent) where
  addReply reply' (InitActionsBuilder actions) = InitActionsBuilder $ (CommonAction $ ReplyAction reply') : actions

instance supportsAddTimeoutInitActionsBuilder :: SupportsAddTimeout (InitActionsBuilder info internal timerName) timerContent where
  addTimeoutAction action (InitActionsBuilder actions) = InitActionsBuilder $ (CommonAction $ TimeoutAction action) : actions

instance supportsNextEventInitActionsBuilder :: SupportsNextEvent (InitActionsBuilder info internal timerName timerContent) (Event info internal timerName timerContent) where
  addNextEvent event (InitActionsBuilder actions) = InitActionsBuilder $ (NextEvent event) : actions

type StateEnterContext :: forall k1 k2 k3 k4 k5 k6 k7. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> Type
type StateEnterContext info internal timerName timerContent commonData stateId state
  = { context :: Context info internal timerName timerContent commonData stateId state
    , changed :: Boolean
    }

newtype StateEnterActionsBuilder timerName timerContent
  = StateEnterActionsBuilder (List (StateEnterAction timerName timerContent))

newtype StateEnterT :: forall k1 k2 k3 k4 k5 k6 k7. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> (Type -> Type) -> Type -> Type
newtype StateEnterT info internal timerName timerContent commonData stateId state m a
  = StateEnterT (StateT (StateEnterContext info internal timerName timerContent commonData stateId state) m a)

derive newtype instance functorStateEnter :: Functor (StateEnterT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance applyStateEnter :: Apply (StateEnterT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance applicativeStateEnter :: Applicative (StateEnterT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance bindStateEnter :: Bind (StateEnterT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance monadStateEnter :: Monad (StateEnterT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance monadTransStateEnter :: MonadTrans (StateEnterT info internal timerName timerContent commonData stateId state)

--derive instance newtypeStateEnterT :: Newtype (StateEnterT info internal timerName timerContent commonData stateId state m a) _
instance supportsSelfStateEnterT :: SupportsSelf StateEnterT info internal timerName timerContent commonData stateId state where
  self = StateEnterT $ Exports.lift $ selfFFI

instance supportsNewActionsStateEnterActionsBuilder :: SupportsNewActions (StateEnterActionsBuilder timerName timerContent) where
  newActions = StateEnterActionsBuilder nil

instance supportsReplyStateEnterActionsBuilder :: SupportsReply (StateEnterActionsBuilder timerName timerContent) where
  addReply reply' (StateEnterActionsBuilder actions) = StateEnterActionsBuilder $ (ReplyAction reply') : actions

instance supportsAddTimeoutStateEnterActionsBuilder :: SupportsAddTimeout (StateEnterActionsBuilder timerName) timerContent where
  addTimeoutAction action (StateEnterActionsBuilder actions) = StateEnterActionsBuilder $ (TimeoutAction action) : actions

type EventContext :: forall k1 k2 k3 k4 k5 k6 k7. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> Type
type EventContext info internal timerName timerContent commonData stateId state
  = { context :: Context info internal timerName timerContent commonData stateId state
    , changed :: Boolean
    }

newtype EventActionsBuilder info internal timerName timerContent
  = EventActionsBuilder (List (EventAction info internal timerName timerContent))

newtype EventT :: forall k1 k2 k3 k4 k5 k6 k7. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> (Type -> Type) -> Type -> Type
newtype EventT info internal timerName timerContent commonData stateId state m a
  = EventT (StateT (EventContext info internal timerName timerContent commonData stateId state) m a)

derive newtype instance functorEvent :: Functor (EventT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance applyEvent :: Apply (EventT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance applicativeEvent :: Applicative (EventT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance bindEvent :: Bind (EventT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance monadEvent :: Monad (EventT info internal timerName timerContent commonData stateId state Effect)

derive newtype instance monadTransEvent :: MonadTrans (EventT info internal timerName timerContent commonData stateId state)

--derive instance newtypeEventT :: Newtype (EventT info internal timerName timerContent commonData stateId state m a) _
instance supportsSelfEventT :: SupportsSelf EventT info internal timerName timerContent commonData stateId state where
  self = EventT $ Exports.lift $ selfFFI

instance supportsNewActionsEventActionsBuilder :: SupportsNewActions (EventActionsBuilder info internal timerName timerContent) where
  newActions = EventActionsBuilder nil

instance supportsReplyEventActionsBuilder :: SupportsReply (EventActionsBuilder info internal timerName timerContent) where
  addReply reply' (EventActionsBuilder actions) = EventActionsBuilder $ (CommonAction (ReplyAction reply')) : actions

instance supportsNextEventEventActionsBuilder :: SupportsNextEvent (EventActionsBuilder info internal timerName timerContent) (Event info internal timerName timerContent) where
  addNextEvent event (EventActionsBuilder actions) = EventActionsBuilder $ (NextEvent event) : actions

newtype StatemType :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype StatemType info internal timerName timerContent commonData stateId state
  = StatemType Void

newtype StatemPid :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype StatemPid info internal timerName timerContent commonData stateId state
  = StatemPid (Process info)

derive newtype instance statemPidHasPid :: HasPid (StatemPid info internal timerName timerContent commonData stateId state)

derive newtype instance statemPidHasProcess :: HasProcess info (StatemPid info internal timerName timerContent commonData stateId state)

type StatemRef info internal timerName timerContent commonData stateId state
  = RegistryReference (StatemPid info internal timerName timerContent commonData stateId state)
      (StatemType info internal timerName timerContent commonData stateId state)

type StatemInstance info internal timerName timerContent commonData stateId state
  = RegistryInstance (StatemPid info internal timerName timerContent commonData stateId state)
      (StatemType info internal timerName timerContent commonData stateId state)

type Spec info internal timerName timerContent commonData stateId state
  = { name :: Maybe (RegistryName (StatemType info internal timerName timerContent commonData stateId state))
    , init :: InitFn info internal timerName timerContent commonData stateId state
    , handleEvent :: HandleEventFn info internal timerName timerContent commonData stateId state
    , handleEnter :: Maybe (EnterFn info internal timerName timerContent commonData stateId state)
    , terminate :: Maybe (TerminateFn info internal timerName timerContent commonData stateId state)
    , trapExits :: Maybe (ExitMessage -> info)
    , getStateId :: state -> stateId
    }

type InitFn :: forall k. Type -> Type -> Type -> Type -> Type -> k -> Type -> Type
type InitFn info internal timerName timerContent commonData stateId state
  = InitT info internal timerName timerContent commonData stateId state Effect (InitResult info internal timerName timerContent commonData state)

type CastFn :: forall k. Type -> Type -> Type -> Type -> Type -> k -> Type -> Type
type CastFn info internal timerName timerContent commonData stateId state
  = EventFn info internal timerName timerContent commonData stateId state

type CallFn :: forall k. k -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
type CallFn reply info internal timerName timerContent commonData stateId state
  = From reply ->
    EventFn info internal timerName timerContent commonData stateId state

type HandleEventFn :: forall k. Type -> Type -> Type -> Type -> Type -> k -> Type -> Type
type HandleEventFn info internal timerName timerContent commonData stateId state
  = Event info internal timerName timerContent ->
    EventFn info internal timerName timerContent commonData stateId state

type EventFn :: forall k. Type -> Type -> Type -> Type -> Type -> k -> Type -> Type
type EventFn info internal timerName timerContent commonData stateId state
  = state ->
    commonData ->
    EventT info internal timerName timerContent commonData stateId state Effect (EventResult info internal timerName timerContent commonData state)

type TerminateFn :: forall k. Type -> Type -> Type -> Type -> Type -> k -> Type -> Type
type TerminateFn info internal timerName timerContent commonData stateId state
  = ShutdownReason ->
    state ->
    commonData ->
    EventT info internal timerName timerContent commonData stateId state Effect Unit

type EnterFn :: forall k1 k2. k1 -> k2 -> Type -> Type -> Type -> Type -> Type -> Type
type EnterFn info internal timerName timerContent commonData stateId state
  = stateId ->
    stateId ->
    state ->
    commonData ->
    StateEnterT info internal timerName timerContent commonData stateId state Effect (StateEnterResult timerName timerContent commonData)

data EventResult info internal timerName timerContent commonData state
  = EventKeepStateAndData
  | EventKeepStateAndDataWithActions (EventActionsBuilder info internal timerName timerContent)
  | EventKeepState commonData
  | EventKeepStateWithActions commonData (EventActionsBuilder info internal timerName timerContent)
  | EventNextState state commonData
  | EventNextStateWithActions state commonData (EventActionsBuilder info internal timerName timerContent)
  | EventStopNormal

data StateEnterResult timerName timerContent commonData
  = StateEnterOk commonData
  | StateEnterOkWithActions commonData (StateEnterActionsBuilder timerName timerContent)
  | StateEnterKeepData
  | StateEnterKeepDataWithActions (StateEnterActionsBuilder timerName timerContent)

data InitResult info internal timerName timerContent commonData state
  = InitOk state commonData
  | InitOkWithActions state commonData (InitActionsBuilder info internal timerName timerContent)
  | InitStop Foreign
  | InitIgnore

type StateEnterAction timerName timerContent
  = CommonAction timerName timerContent

-- This is what Erlang calls an action, and is supported
-- for handle_event (not for state enter though) and for
-- init
data EventAction info internal timerName timerContent
  = CommonAction (CommonAction timerName timerContent)
  | Postpone
  | NextEvent (Event info internal timerName timerContent)

data Event info internal timerName timerContent
  = EventInfo info
  | EventInternal internal
  | EventTimeout timerContent
  | EventNamedTimeout timerName timerContent
  | EventStateTimeout timerContent

-- This is what erlang calls a state_enter_action, and is returnable
-- from state enter, as well as by handle_event and init
data CommonAction timerName timerContent
  = Hibernate
  | TimeoutAction (TimeoutAction timerContent)
  | NamedTimeoutAction (NamedTimeoutAction timerName timerContent)
  | ReplyAction Reply

data TimeoutAction timerContent
  = SetTimeout (Timeout timerContent)
  | SetStateTimeout (Timeout timerContent)
  | UpdateTimeout timerContent
  | UpdateStateTimeout timerContent

data NamedTimeoutAction timerName timerContent
  = SetNamedTimeout timerName (Timeout timerContent)
  | UpdateNamedTimeout timerName timerContent
  | CancelNamedTimeout

data Timeout timerContent
  = At Int timerContent
  | After Int timerContent
  | Cancel

type Context :: forall k1 k2 k3 k4 k5 k6 k7. k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> Type
type Context info internal timerName timerContent commonData stateId state
  = {
    }

newtype OuterData info internal timerName timerContent commonData stateId state
  = OuterData
  { state :: state
  , commonData :: commonData
  , trapExits :: Maybe (ExitMessage -> info)
  , handleEnter :: EnterFn info internal timerName timerContent commonData stateId state
  , handleEvent :: HandleEventFn info internal timerName timerContent commonData stateId state
  , terminate :: Maybe (TerminateFn info internal timerName timerContent commonData stateId state)
  , context :: Context info internal timerName timerContent commonData stateId state
  , getStateId :: state -> stateId
  }

data OuterInitResult info internal timerName timerContent commonData stateId state
  = OuterInitOk stateId (OuterData info internal timerName timerContent commonData stateId state)
  | OuterInitOkWithActions stateId (OuterData info internal timerName timerContent commonData stateId state) (InitActionsBuilder info internal timerName timerContent)
  | OuterInitStop Foreign
  | OuterInitIgnore

data OuterStateEnterResult info internal timerName timerContent commonData stateId state
  = OuterStateEnterOk (OuterData info internal timerName timerContent commonData stateId state)
  | OuterStateEnterOkWithActions (OuterData info internal timerName timerContent commonData stateId state) (StateEnterActionsBuilder timerName timerContent)
  | OuterStateEnterKeepData
  | OuterStateEnterKeepDataWithActions (StateEnterActionsBuilder timerName timerContent)

data OuterEventResult info internal timerName timerContent commonData stateId state
  = OuterEventKeepStateAndData
  | OuterEventKeepStateAndDataWithActions (EventActionsBuilder info internal timerName timerContent)
  | OuterEventKeepState (OuterData info internal timerName timerContent commonData stateId state)
  | OuterEventKeepStateWithActions (OuterData info internal timerName timerContent commonData stateId state) (EventActionsBuilder info internal timerName timerContent)
  | OuterEventNextState stateId (OuterData info internal timerName timerContent commonData stateId state)
  | OuterEventNextStateWithActions stateId (OuterData info internal timerName timerContent commonData stateId state) (EventActionsBuilder info internal timerName timerContent)
  | OuterEventStopNormal

foreign import data Reply :: Type

foreign import data FromForeign :: Type

newtype From :: forall k. k -> Type
newtype From reply
  = From FromForeign

foreign import startLinkFFI ::
  forall info internal timerName timerContent commonData stateId state.
  Maybe (RegistryName (StatemType info internal timerName timerContent commonData stateId state)) ->
  NativeModuleName ->
  Spec info internal timerName timerContent commonData stateId state ->
  Effect (StartLinkResult (StatemPid info internal timerName timerContent commonData stateId state))

foreign import procLibStartLinkFFI ::
  forall info internal timerName timerContent commonData stateId state.
  Maybe (RegistryName (StatemType info internal timerName timerContent commonData stateId state)) ->
  NativeModuleName ->
  Spec info internal timerName timerContent commonData stateId state ->
  Effect (StartLinkResult (StatemPid info internal timerName timerContent commonData stateId state))

foreign import selfFFI ::
  forall info internal timerName timerContent commonData stateId state.
  Effect (StatemPid info internal timerName timerContent commonData stateId state)

foreign import reply :: forall reply. From reply -> reply -> Reply

foreign import callFFI ::
  forall reply info internal timerName timerContent commonData stateId state.
  StatemInstance info internal timerName timerContent commonData stateId state ->
  CallFn reply info internal timerName timerContent commonData stateId state ->
  Effect reply

foreign import castFFI ::
  forall info internal timerName timerContent commonData stateId state.
  StatemInstance info internal timerName timerContent commonData stateId state ->
  CastFn info internal timerName timerContent commonData stateId state ->
  Effect Unit

startLink ::
  forall info internal timerName timerContent commonData stateId state.
  HasStateId stateId state =>
  Spec info internal timerName timerContent commonData stateId state ->
  Effect (StartLinkResult (StatemPid info internal timerName timerContent commonData stateId state))
startLink args@{ name: maybeName } = startLinkFFI maybeName (nativeModuleName pintoGenStatem) args

procLibStartLink ::
  forall info internal timerName timerContent commonData stateId state.
  HasStateId stateId state =>
  Spec info internal timerName timerContent commonData stateId state ->
  Effect (StartLinkResult (StatemPid info internal timerName timerContent commonData stateId state))
procLibStartLink args@{ name: maybeName } = procLibStartLinkFFI maybeName (nativeModuleName pintoGenStatem) args

-- enterLoop ::
--   forall info internal timerName timerContent commonData stateId state.
--   HasStateId stateId state =>
--   Spec info internal timerName timerContent commonData stateId state -> Effect Unit
-- enterLoop = enterLoopFFI (nativeModuleName pintoGenStatem)
defaultSpec ::
  forall info internal timerName timerContent commonData stateId state.
  HasStateId stateId state =>
  InitFn info internal timerName timerContent commonData stateId state ->
  HandleEventFn info internal timerName timerContent commonData stateId state ->
  Spec info internal timerName timerContent commonData stateId state
defaultSpec initFn handleEventFn =
  { name: Nothing
  , trapExits: Nothing
  , init: initFn
  , handleEvent: handleEventFn
  , handleEnter: Nothing
  , terminate: Nothing
  , getStateId: getStateId
  }

call ::
  forall reply info internal timerName timerContent commonData stateId state.
  HasStateId stateId state =>
  StatemRef info internal timerName timerContent commonData stateId state ->
  CallFn reply info internal timerName timerContent commonData stateId state ->
  Effect reply
call r callFn = callFFI (registryInstance r) callFn

cast ::
  forall info internal timerName timerContent commonData stateId state.
  HasStateId stateId state =>
  StatemRef info internal timerName timerContent commonData stateId state ->
  CastFn info internal timerName timerContent commonData stateId state ->
  Effect Unit
cast r castFn = castFFI (registryInstance r) castFn

runEventFn ::
  forall info internal timerName timerContent commonData stateId state.
  (state -> stateId) ->
  EventFn info internal timerName timerContent commonData stateId state ->
  OuterData info internal timerName timerContent commonData stateId state ->
  Effect (OuterEventResult info internal timerName timerContent commonData stateId state)
runEventFn getStateId eventFn (OuterData outerData@{ state, commonData, context }) = do
  let
    (EventT stateT) = eventFn state commonData
  result <- StateT.runStateT stateT { context, changed: false }
  let
    result' = Tuple.fst result
  let
    { context: newContext, changed: contextChanged } = Tuple.snd result
  case result' of
    EventKeepStateAndData ->
      if contextChanged then
        pure $ OuterEventKeepState (OuterData $ outerData { context = newContext })
      else
        pure $ OuterEventKeepStateAndData
    EventKeepStateAndDataWithActions actions ->
      if contextChanged then
        pure $ OuterEventKeepStateWithActions (OuterData $ outerData { context = newContext }) actions
      else
        pure $ OuterEventKeepStateAndDataWithActions actions
    EventKeepState newData -> pure $ OuterEventKeepState (OuterData $ outerData { commonData = newData, context = newContext })
    EventKeepStateWithActions newData actions -> pure $ OuterEventKeepStateWithActions (OuterData $ outerData { commonData = newData, context = newContext }) actions
    EventNextState newState newData -> pure $ OuterEventNextState (getStateId newState) (OuterData $ outerData { state = newState, commonData = newData, context = newContext })
    EventNextStateWithActions newState newData actions -> pure $ OuterEventNextStateWithActions (getStateId newState) (OuterData $ outerData { state = newState, commonData = newData, context = newContext }) actions
    EventStopNormal -> pure $ OuterEventStopNormal

-- NOTE: we don't need to check whether the new state id matches the old one, Erlang does that, it treats things -- like keep_state_and_data as a synonym for {next_state, OldState, OldData}
-- GenStatem API
callback_mode :: List Atom
callback_mode = (atom "handle_event_function") : (atom "state_enter") : nil

init ::
  forall info internal timerName timerContent commonData stateId state.
  EffectFn1 (Spec info internal timerName timerContent commonData stateId state) NativeInitResult
init =
  mkEffectFn1 \{ init: (InitT f)
  , handleEnter: maybeHandleEnter
  , handleEvent
  , trapExits
  , terminate: terminate'
  , getStateId
  } -> do
    _ <- case trapExits of
      Nothing -> pure unit
      Just _ -> void $ setProcessFlagTrapExit true
    result <- StateT.runStateT f {}
    let
      result' = Tuple.fst result

      contextResult = Tuple.snd result

      mkOuterData state commonData innerContext =
        OuterData
          { state
          , commonData
          , handleEnter: fromMaybe (\_ _ _ _ -> pure StateEnterKeepData) maybeHandleEnter
          , handleEvent
          , terminate: terminate'
          , trapExits
          , context: innerContext
          , getStateId: getStateId
          }
    pure
      $ export
      $ case result' of
          (InitOk state commonData) -> OuterInitOk (getStateId state) (mkOuterData state commonData contextResult)
          (InitOkWithActions state commonData actions) -> OuterInitOkWithActions (getStateId state) (mkOuterData state commonData contextResult) actions
          (InitStop error) -> OuterInitStop error
          (InitIgnore) -> OuterInitIgnore

runEnterState ::
  forall info internal timerName timerContent commonData stateId state.
  EnterFn info internal timerName timerContent commonData stateId state ->
  stateId ->
  stateId ->
  OuterData info internal timerName timerContent commonData stateId state ->
  Effect (OuterStateEnterResult info internal timerName timerContent commonData stateId state)
runEnterState handleEnter oldStateId newStateId (OuterData currentData@{ state, commonData, context }) = do
  let
    (StateEnterT stateT) = handleEnter oldStateId newStateId state commonData
  result <- StateT.runStateT stateT { context, changed: false }
  let
    result' = Tuple.fst result

    { context: newContext, changed: contextChanged } = Tuple.snd result
  case result' of
    StateEnterOk newData -> pure $ OuterStateEnterOk (OuterData $ currentData { commonData = newData })
    StateEnterOkWithActions newData actions -> pure $ OuterStateEnterOkWithActions (OuterData $ currentData { commonData = newData }) actions
    StateEnterKeepData ->
      if contextChanged then
        pure $ OuterStateEnterOk (OuterData $ currentData { context = newContext })
      else
        pure $ OuterStateEnterKeepData
    StateEnterKeepDataWithActions actions ->
      if contextChanged then
        pure $ OuterStateEnterOkWithActions (OuterData $ currentData { context = newContext }) actions
      else
        pure $ OuterStateEnterKeepDataWithActions actions

handle_event ::
  forall info internal timerName timerContent commonData stateId state.
  EffectFn4 Foreign Foreign stateId (OuterData info internal timerName timerContent commonData stateId state) NativeHandleEventResult
handle_event =
  mkEffectFn4 \t e stateId dat@(OuterData { getStateId, handleEvent, handleEnter, trapExits }) -> do
    let
      parseTe :: Foreign -> info
      parseTe =
        ( \nativeMsg ->
            fromMaybe' (\_ -> assumeExpectedMessage nativeMsg) $ trapExits <*> (parseTrappedExitFFI nativeMsg Exit)
        )
    case parseEventFFI t parseTe e of
      HandleEventEnter oldStateId -> export <$> runEnterState handleEnter oldStateId stateId dat
      HandleEventCall from f -> export <$> runEventFn getStateId (f from) dat
      HandleEventCast f -> export <$> runEventFn getStateId f dat
      HandleEvent ev -> export <$> runEventFn getStateId (handleEvent ev) dat

assumeExpectedMessage :: forall msg. Foreign -> msg
assumeExpectedMessage = unsafeCoerce

terminate ::
  forall info internal timerName timerContent commonData stateId state.
  EffectFn3 Foreign stateId (OuterData info internal timerName timerContent commonData stateId state) Atom
terminate =
  mkEffectFn3 \reason _stateId (OuterData { terminate: maybeTerminate, state, commonData, context }) -> do
    case maybeTerminate of
      Just f -> do
        let (EventT stateT) = f (parseShutdownReasonFFI reason) state commonData
        _ <- StateT.runStateT stateT { context, changed: false }
        pure unit
      Nothing -> pure unit
    pure $ atom "ok"

data HandleEvent :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data HandleEvent reply info internal timerName timerContent commonData stateId state
  = HandleEventEnter stateId
  | HandleEventCall (From reply) (CallFn reply info internal timerName timerContent commonData stateId state)
  | HandleEventCast (CastFn info internal timerName timerContent commonData stateId state)
  | HandleEvent (Event info internal timerName timerContent)

foreign import parseEventFFI ::
  forall reply info internal timerName timerContent commonData stateId state.
  Foreign -> (Foreign -> info) -> Foreign -> HandleEvent reply info internal timerName timerContent commonData stateId state

foreign import data NativeInitResult :: Type

foreign import data NativeAction :: Type

foreign import data NativeHandleEventResult :: Type

instance exportOuterStateEnterResult :: ExportsTo (OuterStateEnterResult info internal timerName timerContent commonData stateId state) NativeHandleEventResult where
  export = case _ of
    OuterStateEnterOk newData -> unsafeCoerce $ tuple2 (atom "keep_state") newData
    OuterStateEnterOkWithActions newData actions -> unsafeCoerce $ tuple3 (atom "keep_state") newData (export actions :: List NativeAction)
    OuterStateEnterKeepData -> unsafeCoerce $ atom "keep_state_and_data"
    OuterStateEnterKeepDataWithActions actions -> unsafeCoerce $ tuple2 (atom "keep_state_and_data") (export actions :: List NativeAction)

instance exportOuterEventResult :: ExportsTo (OuterEventResult info internal timerName timerContent commonData stateId state) NativeHandleEventResult where
  export = case _ of
    OuterEventKeepStateAndData -> unsafeCoerce $ atom "keep_state_and_data"
    OuterEventKeepStateAndDataWithActions actions -> unsafeCoerce $ tuple2 (atom "keep_state_and_data") (export actions :: List NativeAction)
    OuterEventKeepState newData -> unsafeCoerce $ tuple2 (atom "keep_state") newData
    OuterEventKeepStateWithActions newData actions -> unsafeCoerce $ tuple3 (atom "keep_state") newData (export actions :: List NativeAction)
    OuterEventNextState newState newData -> unsafeCoerce $ unsafeCoerce $ tuple3 (atom "next_state") newState newData
    OuterEventNextStateWithActions newState newData actions -> unsafeCoerce $ tuple4 (atom "next_state") newState newData (export actions :: List NativeAction)
    OuterEventStopNormal -> unsafeCoerce $ atom "stop"

instance exportOuterInitResult :: ExportsTo (OuterInitResult info internal timerName timerContent commonData stateId state) NativeInitResult where
  export = case _ of
    OuterInitOk state dat -> unsafeCoerce $ tuple3 (atom "ok") state dat
    OuterInitOkWithActions state dat actions -> unsafeCoerce $ tuple4 (atom "ok") state dat $ (export actions :: List NativeAction)
    OuterInitStop err -> unsafeCoerce $ tuple2 (atom "stop") err
    OuterInitIgnore -> unsafeCoerce $ atom "ignore"

instance exportActions :: ExportsTo (InitActionsBuilder a b c d) (List NativeAction) where
  export (InitActionsBuilder actions) = export <$> actions

instance exportEventActions :: ExportsTo (EventActionsBuilder a b c d) (List NativeAction) where
  export (EventActionsBuilder actions) = export <$> actions

instance exportStateEnterActions :: ExportsTo (StateEnterActionsBuilder a b) (List NativeAction) where
  export (StateEnterActionsBuilder actions) = export <$> actions

instance exportEventAction :: ExportsTo (EventAction a b c d) NativeAction where
  export = case _ of
    CommonAction action -> export action
    Postpone -> unsafeCoerce $ atom "postpone"
    NextEvent event -> export event

instance exportEvent :: ExportsTo (Event a b c d) NativeAction where
  export = case _ of
    EventInfo info -> unsafeCoerce $ tuple3 (atom "next_event") (atom "info") info
    EventInternal internal -> unsafeCoerce $ tuple3 (atom "next_event") (atom "internal") internal
    EventTimeout timerContent -> unsafeCoerce $ tuple3 (atom "next_event") (atom "timeout") timerContent
    EventNamedTimeout timerName timerContent -> unsafeCoerce $ tuple3 (atom "next_event") (tuple2 (atom "timeout") timerName) timerContent
    EventStateTimeout timerContent -> unsafeCoerce $ tuple3 (atom "next_event") (atom "state_timeout") timerContent

instance exportCommonAction :: ExportsTo (CommonAction a b) NativeAction where
  export = case _ of
    Hibernate -> unsafeCoerce $ atom "hibernate"
    TimeoutAction timeout -> export timeout
    NamedTimeoutAction _named -> unsafeCrashWith "Not implemented"
    ReplyAction reply' -> unsafeCoerce reply'

instance exportTimeoutAction :: ExportsTo (TimeoutAction a) NativeAction where
  export = case _ of
    SetTimeout (At timeout content) -> unsafeCoerce $ tuple4 (atom "timeout") timeout content ((tuple2 (atom "abs") true : nil))
    SetTimeout (After timeout content) -> unsafeCoerce $ tuple3 (atom "timeout") timeout content
    SetTimeout Cancel -> unsafeCoerce $ tuple2 (atom "timeout") (atom "cancel")
    SetStateTimeout (At timeout content) -> unsafeCoerce $ tuple4 (atom "state_timeout") timeout content ((tuple2 (atom "abs") true : nil))
    SetStateTimeout (After timeout content) -> unsafeCoerce $ tuple3 (atom "state_timeout") timeout content
    SetStateTimeout Cancel -> unsafeCoerce $ tuple2 (atom "state_timeout") (atom "cancel")
    UpdateTimeout content -> unsafeCoerce $ tuple3 (atom "timeout") (atom "update") content
    UpdateStateTimeout content -> unsafeCoerce $ tuple3 (atom "state_timeout") (atom "update") content
