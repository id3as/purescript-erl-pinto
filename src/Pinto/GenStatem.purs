module Pinto.GenStatem
       ( StatemPid
       , StatemRef(..)

       , class IsStatemType

       , class HasStateId
       , getStateId

       , class SupportsReply
       , addReply

       , class SupportsAddTimeout
       , addTimeoutAction

       , class SupportsSelf
       , self

       , class SupportsNewActions
       , newActions

       , class SupportsMonitor
       , monitor
       , demonitor
       , MonitorRef
       , DownReason

       , InitFn
       , InitResult(..)
       , InitT
       , InitActionsBuilder

       , CastFn
       , CallFn
       , HandleEventFn
       , EventFn
       , EventResult(..)
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
       , mkReply

       , startLink
       , mkSpec
       , call
       , cast

       , module Exports
       )
       where

import Prelude

import Control.Monad.State.Trans (StateT)
import Control.Monad.State.Trans as StateT
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Trans.Class (lift) as Exports

import Data.Function.Uncurried (Fn1, Fn2, Fn3, mkFn1, mkFn2, mkFn3)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Tuple as Tuple
import Effect (Effect)
import Erl.Data.List (List, (:), nil)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process (Process(..))
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Pinto.Types (RegistryName, StartLinkResult, class HasRawPid, getRawPid, class HasProcess)
-- import Unsafe.Coerce (unsafeCoerce)

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

class SupportsAddTimeout builder timerContent  where
  addTimeoutAction :: TimeoutAction timerContent -> builder timerContent -> builder timerContent

class SupportsSelf context statemType where
  self :: context statemType Effect (StatemPid statemType)

foreign import data MonitorRef :: Type

data DownReason
  = DownNormal
  | DownNoConnection
  | DownOther Foreign

class SupportsMonitor context info internal timerName timerContent commonData stateId state where
  monitor ::
    forall process. HasRawPid process =>
    process ->
    MonitorFn info internal timerName timerContent commonData stateId state ->
    context info internal timerName timerContent commonData stateId state Effect MonitorRef

  demonitor ::
    MonitorRef ->
    context info internal timerName timerContent commonData stateId state Effect Unit

type MonitorFn info internal timerName timerContent commonData stateId state =
  DownReason ->
  EventFn info internal timerName timerContent commonData stateId state

class SupportsNewActions builder where
  newActions :: builder

-- -----------------------------------------------------------------------------
-- InitT
-- -----------------------------------------------------------------------------
type InitContext statemType =
  Context statemType

newtype InitActionsBuilder info internal timerName timerContent =
  InitActionsBuilder (List (EventAction info internal timerName timerContent))

newtype InitT statemType m a =
  InitT (StateT (InitContext statemType) m a)

derive newtype instance functorInit :: Functor (InitT statemType Effect)
derive newtype instance applyInit :: Apply (InitT statemType Effect)
derive newtype instance applicativeInit :: Applicative (InitT statemType Effect)
derive newtype instance bindInit :: Bind (InitT statemType Effect)
derive newtype instance monadInit :: Monad (InitT statemType Effect)
derive newtype instance monadTransInit :: MonadTrans (InitT statemType)

instance supportsSelfInitT :: SupportsSelf InitT statemType where
  self = InitT $ Exports.lift $ selfFFI

instance supportsMonitorInitT :: (HasStateId stateId state) => SupportsMonitor InitT statemType where
  monitor process handleFn =
    InitT $ monitorImpl

    where
      monitorImpl :: (StateT (InitContext statemType) Effect MonitorRef)
      monitorImpl = do
         context <- StateT.get
         { monitorRef, newContext } <- StateT.lift $ monitorFFI (getRawPid process) (wrapMonitorFn handleFn) context
         StateT.put newContext
         pure monitorRef

  demonitor monitorRef =
    InitT $ demonitorImpl

    where
      demonitorImpl :: (StateT (InitContext statemType) Effect Unit)
      demonitorImpl = do
         context <- StateT.get
         newContext <- StateT.lift $ demonitorFFI monitorRef context
         StateT.put newContext
         pure unit

instance supportsNewActionsInitActionsBuilder :: SupportsNewActions (InitActionsBuilder info internal timerName timerContent) where
  newActions = InitActionsBuilder nil

instance supportsReplyInitActionsBuilder :: SupportsReply (InitActionsBuilder info internal timerName timerContent) where
  addReply reply (InitActionsBuilder actions) = InitActionsBuilder $ (CommonAction $ ReplyAction reply) : actions

instance supportsAddTimeoutInitActionsBuilder :: SupportsAddTimeout (InitActionsBuilder info internal timerName) timerContent where
  addTimeoutAction action (InitActionsBuilder actions) = InitActionsBuilder $ (CommonAction $ TimeoutAction action) : actions

-- -----------------------------------------------------------------------------
-- StateEnterT
-- -----------------------------------------------------------------------------
-- TODO: we've not yet modeled things like stop/stop_and_reply, they are a bit interesting
-- because they only admit replies, not other sorts of actions, so do we simply throw away
-- any actions we've accumulated aside from the replies, or throw them all and have a function which is
-- specifically for stop_and_reply? or something else
type StateEnterContext info internal timerName timerContent commonData stateId state =
  { context :: Context info internal timerName timerContent commonData stateId state
  , changed :: Boolean
  }

newtype StateEnterActionsBuilder timerName timerContent =
  StateEnterActionsBuilder (List (StateEnterAction timerName timerContent))

newtype StateEnterT info internal timerName timerContent commonData stateId state m a =
  StateEnterT (StateT (StateEnterContext info internal timerName timerContent commonData stateId state) m a)

derive newtype instance functorStateEnter :: Functor (StateEnterT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance applyStateEnter :: Apply (StateEnterT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance applicativeStateEnter :: Applicative (StateEnterT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance bindStateEnter :: Bind (StateEnterT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance monadStateEnter :: Monad (StateEnterT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance monadTransStateEnter :: MonadTrans (StateEnterT info internal timerName timerContent commonData stateId state)
derive instance newtypeStateEnterT :: Newtype (StateEnterT info internal timerName timerContent commonData stateId state m a) _

instance supportsSelfStateEnterT :: SupportsSelf StateEnterT info internal timerName timerContent commonData stateId state where
  self = StateEnterT $ Exports.lift $ selfFFI

instance supportsMonitorStateEnterT :: (HasStateId stateId state) => SupportsMonitor StateEnterT info internal timerName timerContent commonData stateId state where
  monitor process handleFn =
    StateEnterT $ monitorImpl

    where
      monitorImpl :: (StateT (StateEnterContext info internal timerName timerContent commonData stateId state) Effect MonitorRef)
      monitorImpl = do
         { context } <- StateT.get
         { monitorRef, newContext } <- StateT.lift $ monitorFFI (getRawPid process) (wrapMonitorFn handleFn) context
         StateT.put { context: newContext, changed: true }
         pure monitorRef

  demonitor monitorRef =
    StateEnterT $ demonitorImpl

    where
      demonitorImpl :: (StateT (StateEnterContext info internal timerName timerContent commonData stateId state) Effect Unit)
      demonitorImpl = do
         { context } <- StateT.get
         newContext <- StateT.lift $ demonitorFFI monitorRef context
         StateT.put { context: newContext, changed: true }
         pure unit

instance supportsNewActionsStateEnterActionsBuilder :: SupportsNewActions (StateEnterActionsBuilder timerName timerContent) where
  newActions = StateEnterActionsBuilder nil

instance supportsReplyStateEnterActionsBuilder :: SupportsReply (StateEnterActionsBuilder timerName timerContent) where
  addReply reply (StateEnterActionsBuilder actions) = StateEnterActionsBuilder $ (ReplyAction reply) : actions

instance supportsAddTimeoutStateEnterActionsBuilder :: SupportsAddTimeout (StateEnterActionsBuilder timerName) timerContent where
  addTimeoutAction action (StateEnterActionsBuilder actions) = StateEnterActionsBuilder $ (TimeoutAction action) : actions

-- -----------------------------------------------------------------------------
-- EventT
-- -----------------------------------------------------------------------------
type EventContext info internal timerName timerContent commonData stateId state =
  { context :: Context info internal timerName timerContent commonData stateId state
  , changed :: Boolean
  }

newtype EventActionsBuilder info internal timerName timerContent =
  EventActionsBuilder (List (EventAction info internal timerName timerContent))

newtype EventT info internal timerName timerContent commonData stateId state m a =
  EventT (StateT (EventContext info internal timerName timerContent commonData stateId state) m a)

derive newtype instance functorEvent :: Functor (EventT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance applyEvent :: Apply (EventT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance applicativeEvent :: Applicative (EventT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance bindEvent :: Bind (EventT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance monadEvent :: Monad (EventT info internal timerName timerContent commonData stateId state Effect)
derive newtype instance monadTransEvent :: MonadTrans (EventT info internal timerName timerContent commonData stateId state)
derive instance newtypeEventT :: Newtype (EventT info internal timerName timerContent commonData stateId state m a) _

instance supportsSelfEventT :: SupportsSelf EventT info internal timerName timerContent commonData stateId state where
  self = EventT $ Exports.lift $ selfFFI

instance supportsMonitorEventT :: (HasStateId stateId state) => SupportsMonitor EventT info internal timerName timerContent commonData stateId state where
  monitor process handleFn =
    EventT $ monitorImpl

    where
      monitorImpl :: (StateT (EventContext info internal timerName timerContent commonData stateId state) Effect MonitorRef)
      monitorImpl = do
         { context } <- StateT.get
         { monitorRef, newContext } <- StateT.lift $ monitorFFI (getRawPid process) (wrapMonitorFn handleFn) context
         StateT.put { context: newContext, changed: true }
         pure monitorRef

  demonitor monitorRef =
    EventT $ demonitorImpl

    where
      demonitorImpl :: (StateT (EventContext info internal timerName timerContent commonData stateId state) Effect Unit)
      demonitorImpl = do
         { context } <- StateT.get
         newContext <- StateT.lift $ demonitorFFI monitorRef context
         StateT.put { context: newContext, changed: true }
         pure unit

instance supportsNewActionsEventActionsBuilder :: SupportsNewActions (EventActionsBuilder info internal timerName timerContent) where
  newActions = EventActionsBuilder nil

instance supportsReplyEventActionsBuilder :: SupportsReply (EventActionsBuilder info internal timerName timerContent) where
  addReply reply (EventActionsBuilder actions) = EventActionsBuilder $ (CommonAction (ReplyAction reply)) : actions

-- -----------------------------------------------------------------------------
-- Other Gubbins
-- -----------------------------------------------------------------------------
class (HasStateId stateId state) <= IsStatemType statemType info internal timerName timerContent commonData stateId state |
  statemType -> info,
  statemType -> internal,
  statemType -> timerName,
  statemType -> timerContent,
  statemType -> commonData,
  statemType -> stateId,
  statemType -> state

newtype StatemPid statemType = StatemPid Pid


-- newtype StatemType info internal timerName timerContent commonData stateId state = StatemType Void
-- newtype StatemPid info internal timerName timerContent commonData stateId state = StatemPid (Process info)

derive newtype instance statemPidHasRawPid :: HasRawPid (StatemPid info internal timerName timerContent commonData stateId state)

--  getProcess ::
--    forall statemType info internal timerName timerContent commonData stateId state.
--    IsStatemType statemType info internal timerName timerContent commonData stateId state =>
--    StatemPid statemType ->
--    Process info

instance statemPidHasProcess ::
  IsStatemType statemType info internal timerName timerContent commonData stateId state =>
  HasProcess info (StatemPid statemType) where

  getProcess (StatemPid pid) = Process pid

data StatemRef statemType
  = ByName (RegistryName statemType)
  | ByPid (StatemPid statemType)

type Spec statemType info internal timerName timerContent commonData stateId state =
  IsStatemType statemType info internal timerName timerContent commonData stateId state =>
  { name :: Maybe (statemType)
  , init :: InitFn info internal timerName timerContent commonData stateId state
  , handleEvent :: HandleEventFn info internal timerName timerContent commonData stateId state
  , handleEnter :: Maybe (EnterFn info internal timerName timerContent commonData stateId state)
  }

type InitFn info internal timerName timerContent commonData stateId state =
  InitT info internal timerName timerContent commonData stateId state Effect (InitResult info internal timerName timerContent commonData state)

type CastFn info internal timerName timerContent commonData stateId state =
  EventFn info internal timerName timerContent commonData stateId state

type CallFn reply info internal timerName timerContent commonData stateId state =
  From reply ->
  EventFn info internal timerName timerContent commonData stateId state

type HandleEventFn info internal timerName timerContent commonData stateId state =
  Event info internal timerName timerContent ->
  EventFn info internal timerName timerContent commonData stateId state

type EventFn info internal timerName timerContent commonData stateId state =
  state ->
  commonData ->
  EventT info internal timerName timerContent commonData stateId state Effect (EventResult info internal timerName timerContent commonData state)

type EnterFn info internal timerName timerContent commonData stateId state =
  stateId ->
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

type StateEnterAction timerName timerContent = CommonAction timerName timerContent

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

data Timeout timerContent
  = At Int timerContent
  | After Int timerContent
  | Cancel

-- -----------------------------------------------------------------------------
-- FFI
-- -----------------------------------------------------------------------------
type Context statemType =
  { -- NOTE: this is managed entirely by FFI
    monitorHandlers :: Map MonitorRef (WrappedMonitorFn statemType)
  }

newtype OuterData info internal timerName timerContent commonData stateId state = OuterData
  { state :: state
  , commonData :: commonData
  , handleEnter :: WrappedEnterFn info internal timerName timerContent commonData stateId state
  , handleEvent :: WrappedHandleEventFn info internal timerName timerContent commonData stateId state
  , context :: Context info internal timerName timerContent commonData stateId state
  }

type WrappedEnterFn info internal timerName timerContent commonData stateId state =
  Fn3
    stateId
    stateId
    (OuterData info internal timerName timerContent commonData stateId state)
    (Effect (OuterStateEnterResult info internal timerName timerContent commonData stateId state))

type WrappedCallFn reply info internal timerName timerContent commonData stateId state =
  Fn2
    (From reply)
    (OuterData info internal timerName timerContent commonData stateId state)
    (Effect (OuterEventResult info internal timerName timerContent commonData stateId state))

type WrappedMonitorFn info internal timerName timerContent commonData stateId state =
  Fn2
    DownReason
    (OuterData info internal timerName timerContent commonData stateId state)
    (Effect (OuterEventResult info internal timerName timerContent commonData stateId state))

wrapMonitorFn ::
  forall info internal timerName timerContent commonData stateId state. HasStateId stateId state =>
  MonitorFn info internal timerName timerContent commonData stateId state ->
  WrappedMonitorFn info internal timerName timerContent commonData stateId state

wrapMonitorFn monitorFn = mkFn2 \reason -> runEventFn (monitorFn reason)

type WrappedCastFn info internal timerName timerContent commonData stateId state =
  Fn1
    (OuterData info internal timerName timerContent commonData stateId state)
    (Effect (OuterEventResult info internal timerName timerContent commonData stateId state))

type WrappedHandleEventFn info internal timerName timerContent commonData stateId state =
  Fn2
    (Event info internal timerName timerContent)
    (OuterData info internal timerName timerContent commonData stateId state)
    (Effect (OuterEventResult info internal timerName timerContent commonData stateId state))

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

foreign import data Reply :: Type
foreign import data FromForeign :: Type
newtype From reply = From FromForeign

foreign import startLinkFFI ::
  forall statemType info internal timerName timerContent commonData stateId state.
  Maybe (RegistryName statemType) ->
  Effect (OuterInitResult info internal timerName timerContent commonData stateId state) ->
  Effect (StartLinkResult (StatemPid statemPid))

foreign import selfFFI ::
  forall info internal timerName timerContent commonData stateId state.
  Effect (StatemPid info internal timerName timerContent commonData stateId state)

foreign import monitorFFI ::
  forall info internal timerName timerContent commonData stateId state.
  Pid ->
  WrappedMonitorFn info internal timerName timerContent commonData stateId state ->
  Context info internal timerName timerContent commonData stateId state ->
  Effect { monitorRef :: MonitorRef, newContext :: Context info internal timerName timerContent commonData stateId state }

foreign import demonitorFFI ::
  forall info internal timerName timerContent commonData stateId state.
  MonitorRef ->
  Context info internal timerName timerContent commonData stateId state ->
  Effect (Context info internal timerName timerContent commonData stateId state)

foreign import mkReply :: forall reply. From reply -> reply -> Reply

foreign import callFFI ::
  forall reply info internal timerName timerContent commonData stateId state.
  StatemRef info internal timerName timerContent commonData stateId state ->
  WrappedCallFn reply info internal timerName timerContent commonData stateId state ->
  Effect reply

foreign import castFFI ::
  forall info internal timerName timerContent commonData stateId state.
  StatemRef info internal timerName timerContent commonData stateId state ->
  WrappedCastFn info internal timerName timerContent commonData stateId state ->
  Effect Unit

startLink ::
  forall statemType info internal timerName timerContent commonData stateId state.
  IsStatemType statemType info internal timerName timerContent commonData stateId state =>
  Spec info internal timerName timerContent commonData stateId state ->
  Effect (StartLinkResult (StatemPid statemType))
startLink
  { name: maybeName
  , init: (InitT init)
  , handleEnter: maybeHandleEnter
  , handleEvent
  } =
  startLinkFFI maybeName initEffect

  where
    initEffect :: Effect (OuterInitResult info internal timerName timerContent commonData stateId state)
    initEffect = do
      let initialContext = { monitorHandlers: Map.empty }

      result <- StateT.runStateT init initialContext

      let result' = Tuple.fst result
      let contextResult = Tuple.snd result

      case result' of
        (InitOk state commonData) ->
          pure $ OuterInitOk (getStateId state) (mkOuterData state commonData contextResult)
        (InitOkWithActions state commonData actions) ->
          pure $ OuterInitOkWithActions (getStateId state) (mkOuterData state commonData contextResult) actions
        (InitStop error) ->
          pure $ OuterInitStop error
        (InitIgnore) ->
          pure $ OuterInitIgnore

    mkOuterData state commonData contextResult =
      OuterData
      { state
      , commonData
      , handleEnter: mkFn3 wrappedHandleEnter
      , handleEvent: mkFn2 (\event -> runEventFn (handleEvent event))
      , context: contextResult
      }

    wrappedHandleEnter oldStateId newStateId (OuterData currentData@{ state, commonData, context }) =
      case maybeHandleEnter of
        Just handleEnter -> do
            let stateT = un StateEnterT $ handleEnter oldStateId newStateId state commonData

            result <- StateT.runStateT stateT { context, changed: false }

            let result' = Tuple.fst result
            let { context: newContext, changed: contextChanged } = Tuple.snd result

            case result' of
              StateEnterOk newData ->
                pure $ OuterStateEnterOk (OuterData $ currentData { commonData = newData })

              StateEnterOkWithActions newData actions ->
                pure $ OuterStateEnterOkWithActions (OuterData $ currentData { commonData = newData }) actions

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

        Nothing ->
            pure $ OuterStateEnterKeepData


mkSpec ::
  forall info internal timerName timerContent commonData stateId state. HasStateId stateId state =>
  InitFn info internal timerName timerContent commonData stateId state ->
  HandleEventFn info internal timerName timerContent commonData stateId state ->
  Spec info internal timerName timerContent commonData stateId state

mkSpec initFn handleEventFn =
  { name: Nothing
  , init: initFn
  , handleEvent: handleEventFn
  , handleEnter: Nothing
  }

call ::
  forall reply info internal timerName timerContent commonData stateId state. HasStateId stateId state =>
  StatemRef info internal timerName timerContent commonData stateId state ->
  CallFn reply info internal timerName timerContent commonData stateId state ->
  Effect reply
call instanceRef callFn =
  callFFI instanceRef (mkFn2 \from -> runEventFn (callFn from))

cast ::
  forall info internal timerName timerContent commonData stateId state. HasStateId stateId state =>
  StatemRef info internal timerName timerContent commonData stateId state ->
  CastFn info internal timerName timerContent commonData stateId state ->
  Effect Unit
cast instanceRef castFn =
  castFFI instanceRef (mkFn1 $ runEventFn castFn)

runEventFn ::
  forall info internal timerName timerContent commonData stateId state. HasStateId stateId state =>
  EventFn info internal timerName timerContent commonData stateId state ->
  OuterData info internal timerName timerContent commonData stateId state ->
  Effect (OuterEventResult info internal timerName timerContent commonData stateId state)

runEventFn eventFn (OuterData outerData@{ state, commonData, context }) = do
  let stateT = un EventT $ eventFn state commonData

  result <- StateT.runStateT stateT { context, changed: false }

  let result' = Tuple.fst result
  let { context: newContext, changed: contextChanged } = Tuple.snd result

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

    EventKeepState newData ->
      pure $ OuterEventKeepState (OuterData $ outerData { commonData = newData, context = newContext })

    EventKeepStateWithActions newData actions ->
      pure $ OuterEventKeepStateWithActions (OuterData $ outerData { commonData = newData, context = newContext }) actions

    EventNextState newState newData ->
      -- NOTE: we don't need to check whether the new state id matches the old one, Erlang does that, it treats things
      -- like keep_state_and_data as a synonym for {next_state, OldState, OldData}
      pure $ OuterEventNextState (getStateId newState) (OuterData $ outerData { state = newState, commonData = newData, context = newContext })

    EventNextStateWithActions newState newData actions ->
      -- NOTE: we don't need to check whether the new state id matches the old one, Erlang does that, it treats things
      -- like keep_state_and_data as a synonym for {next_state, OldState, OldData}
      pure $ OuterEventNextStateWithActions (getStateId newState) (OuterData $ outerData { state = newState, commonData = newData, context = newContext }) actions
