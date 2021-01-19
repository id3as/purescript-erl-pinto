module Pinto.GenStatem
       ( StatemType

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

       , InitFn
       , InitResult(..)
       , InitT
       , InitActionsBuilder

       , CallFn
       , CallResult(..)
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

       , module Exports
       )
       where

import Prelude

import Control.Monad.State.Trans (StateT)
import Control.Monad.State.Trans as StateT
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Trans.Class (lift) as Exports

import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, mkFn1, mkFn2, mkFn3, mkFn4)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (List, (:), nil)
import Foreign (Foreign)
import Pinto.Types (InstanceRef, RegistryName, StartLinkResult, ServerPid)
import Unsafe.Coerce (unsafeCoerce)

-- -----------------------------------------------------------------------------
-- States
-- -----------------------------------------------------------------------------
class HasStateId stateId state where
  getStateId :: state -> stateId

-- -----------------------------------------------------------------------------
-- Response Type Classes
-- -----------------------------------------------------------------------------
class SupportsReply b where
  addReply :: Reply -> b -> b

class SupportsAddTimeout b timerContent  where
  addTimeoutAction :: TimeoutAction timerContent -> b timerContent -> b timerContent

class SupportsSelf m info internal timerName timerContent commonData stateId state where
  self :: m info internal timerName timerContent commonData stateId state Effect (ServerPid (StatemType info internal timerName timerContent commonData stateId state))

class SupportsNewActions builder where
  newActions :: builder

-- -----------------------------------------------------------------------------
-- InitT
-- -----------------------------------------------------------------------------
type InitContext info internal timerName timerContent commonData stateId state = {}

newtype InitActionsBuilder info internal timerName timerContent =
  InitActionsBuilder (List (EventAction info internal timerName timerContent))

newtype InitT info internal timerName timerContent commonData stateId state m a =
  InitT (StateT (InitContext info internal timerName timerContent commonData stateId state) m a)

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
type StateEnterContext info internal timerName timerContent commonData stateId state = {}

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

instance supportsSelfStateEnterT :: SupportsSelf StateEnterT info internal timerName timerContent commonData stateId state where
  self = StateEnterT $ Exports.lift $ selfFFI

instance supportsNewActionsStateEnterActionsBuilder :: SupportsNewActions (StateEnterActionsBuilder timerName timerContent) where
  newActions = StateEnterActionsBuilder nil

instance supportsReplyStateEnterActionsBuilder :: SupportsReply (StateEnterActionsBuilder timerName timerContent) where
  addReply reply (StateEnterActionsBuilder actions) = StateEnterActionsBuilder $ (ReplyAction reply) : actions

instance supportsAddTimeoutStateEnterActionsBuilder :: SupportsAddTimeout (StateEnterActionsBuilder timerName) timerContent where
  addTimeoutAction action (StateEnterActionsBuilder actions) = StateEnterActionsBuilder $ (TimeoutAction action) : actions

-- -----------------------------------------------------------------------------
-- EventT
-- -----------------------------------------------------------------------------
type EventContext info internal timerName timerContent commonData stateId state = {}

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

instance supportsSelfEventT :: SupportsSelf EventT info internal timerName timerContent commonData stateId state where
  self = EventT $ Exports.lift $ selfFFI

instance supportsNewActionsEventActionsBuilder :: SupportsNewActions (EventActionsBuilder info internal timerName timerContent) where
  newActions = EventActionsBuilder nil

instance supportsReplyEventActionsBuilder :: SupportsReply (EventActionsBuilder info internal timerName timerContent) where
  addReply reply (EventActionsBuilder actions) = EventActionsBuilder $ (CommonAction (ReplyAction reply)) : actions

-- -----------------------------------------------------------------------------
-- Other Gubbins
-- -----------------------------------------------------------------------------
newtype StatemType info internal timerName timerContent commonData stateId state = StatemType Void

type Spec info internal timerName timerContent commonData stateId state =
  { name :: Maybe (RegistryName (StatemType info internal timerName timerContent commonData stateId state))
  , init :: InitFn info internal timerName timerContent commonData stateId state
  , handleEvent :: EventFn info internal timerName timerContent commonData stateId state
  , handleEnter :: Maybe (EnterFn info internal timerName timerContent commonData stateId state)
  }

type InitFn info internal timerName timerContent commonData stateId state =
  InitT info internal timerName timerContent commonData stateId state Effect (InitResult info internal timerName timerContent commonData state)

type CallFn reply info internal timerName timerContent commonData stateId state =
  From reply ->
  state ->
  commonData ->
  EventT info internal timerName timerContent commonData stateId state Effect (CallResult info internal timerName timerContent commonData state)

type EventFn info internal timerName timerContent commonData stateId state =
  (Event info internal timerName timerContent) ->
  state ->
  commonData ->
  EventT info internal timerName timerContent commonData stateId state Effect (EventResult info internal timerName timerContent commonData state)

type EnterFn info internal timerName timerContent commonData stateId state =
  stateId ->
  stateId ->
  state ->
  commonData ->
  StateEnterT info internal timerName timerContent commonData stateId state Effect (StateEnterResult timerName timerContent commonData)

data CallResult info internal timerName timerContent commonData state
  = CallKeepStateAndData
  | CallKeepStateAndDataWithActions (EventActionsBuilder info internal timerName timerContent)
  | CallKeepState commonData
  | CallKeepStateWithActions commonData (EventActionsBuilder info internal timerName timerContent)
  | CallNextState state commonData
  | CallNextStateWithActions state commonData (EventActionsBuilder info internal timerName timerContent)

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
newtype OuterData info internal timerName timerContent commonData stateId state = OuterData
  { state :: state
  , commonData :: commonData
  , handleEnter :: WrappedEnterFn info internal timerName timerContent commonData stateId state
  }

type WrappedEnterFn info internal timerName timerContent commonData stateId state =
  Fn3
    stateId
    stateId
    (OuterData info internal timerName timerContent commonData stateId state)
    (Effect (OuterStateEnterResult info internal timerName timerContent commonData stateId state))

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

foreign import data Reply :: Type
foreign import data FromForeign :: Type
newtype From reply = From FromForeign

foreign import startLinkFFI ::
  forall info internal timerName timerContent commonData stateId state.
  Maybe (RegistryName (StatemType info internal timerName timerContent commonData stateId state)) ->
  Effect (OuterInitResult info internal timerName timerContent commonData stateId state) ->
  Effect (StartLinkResult (StatemType info internal timerName timerContent commonData stateId state))

foreign import selfFFI :: forall info internal timerName timerContent commonData stateId state. Effect (ServerPid (StatemType info internal timerName timerContent commonData stateId state))

foreign import mkReply :: forall reply. From reply -> reply -> Reply

-- type StatemData = {
--   spec :: Spec info internal timerName timerContent commonData stateId state
--   state :: state
--   commonData :: commonData
--   }

startLink ::
  forall info internal timerName timerContent commonData stateId state. HasStateId stateId state =>
  Spec info internal timerName timerContent commonData stateId state ->
  Effect (StartLinkResult (StatemType info internal timerName timerContent commonData stateId state))
startLink
  { name: maybeName
  , init: (InitT init)
  , handleEnter: maybeHandleEnter
  } =
  startLinkFFI maybeName initEffect

  where
    initEffect :: Effect (OuterInitResult info internal timerName timerContent commonData stateId state)
    initEffect = do
      let context = {}
      result <- StateT.evalStateT init context

      case result of
        (InitOk state commonData) ->
          pure $  OuterInitOk (getStateId state) (OuterData { state, commonData, handleEnter: mkFn3 wrappedHandleEnter })
        (InitOkWithActions state commonData actions) ->
          pure $  OuterInitOkWithActions (getStateId state) (OuterData { state, commonData, handleEnter: mkFn3 wrappedHandleEnter }) actions
        (InitStop error) ->
          pure $  OuterInitStop error
        (InitIgnore) ->
          pure $  OuterInitIgnore

    wrappedHandleEnter oldStateId newStateId (OuterData currentData@{ state, commonData }) =
      case maybeHandleEnter of
        Just handleEnter -> do
            let context = {}
            let StateEnterT stateT = handleEnter oldStateId newStateId state commonData

            result <- StateT.evalStateT stateT context

            case result of
              StateEnterOk newData ->
                pure $ OuterStateEnterOk (OuterData $ currentData { commonData = newData })

              StateEnterOkWithActions newData actions ->
                pure $ OuterStateEnterOkWithActions (OuterData $ currentData { commonData = newData }) actions

              StateEnterKeepData ->
                pure $ OuterStateEnterKeepData

              StateEnterKeepDataWithActions actions ->
                pure $ OuterStateEnterKeepDataWithActions actions

        Nothing ->
            pure $ OuterStateEnterKeepData

mkSpec ::
  forall info internal timerName timerContent commonData stateId state. HasStateId stateId state =>
  InitFn info internal timerName timerContent commonData stateId state ->
  EventFn info internal timerName timerContent commonData stateId state ->
  Spec info internal timerName timerContent commonData stateId state

mkSpec initFn handleEventFn =
  { name: Nothing
  , init: initFn
  , handleEvent: handleEventFn
  , handleEnter: Nothing
  }

call :: forall reply info internal timerName timerContent commonData stateId state. InstanceRef (StatemType info internal timerName timerContent commonData stateId state) -> CallFn reply info internal timerName timerContent commonData stateId state -> Effect reply
call _instanceRef _callFn = unsafeCoerce unit
