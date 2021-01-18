module Pinto.GenStatem
       ( StatemType

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

import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Trans.Class (lift) as Exports

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (List, (:), nil)
import Foreign (Foreign)
import Pinto.Types (InstanceRef, RegistryName, StartLinkResult, ServerPid)
import Unsafe.Coerce (unsafeCoerce)

-- -----------------------------------------------------------------------------
-- FFI
-- -----------------------------------------------------------------------------
foreign import selfFFI :: forall info internal timerName timerContent state stateData . Effect (ServerPid (StatemType info internal timerName timerContent state stateData))

-- -----------------------------------------------------------------------------
-- Response Type Classes
-- -----------------------------------------------------------------------------
class SupportsReply b where
  addReply :: Reply -> b -> b

class SupportsAddTimeout b timerContent  where
  addTimeoutAction :: TimeoutAction timerContent -> b timerContent -> b timerContent

class SupportsSelf m info internal timerName timerContent state stateData where
  self :: m info internal timerName timerContent state stateData Effect (ServerPid (StatemType info internal timerName timerContent state stateData))

class SupportsNewActions builder where
  newActions :: builder

-- -----------------------------------------------------------------------------
-- InitT
-- -----------------------------------------------------------------------------
type InitContext info internal timerName timerContent state stateData = {}

newtype InitActionsBuilder info internal timerName timerContent =
  InitActionsBuilder (List (EventAction info internal timerName timerContent))

newtype InitT info internal timerName timerContent state stateData m a =
  InitT (StateT (InitContext info internal timerName timerContent state stateData) m a)

derive newtype instance functorInit :: Functor (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance applyInit :: Apply (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance applicativeInit :: Applicative (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance bindInit :: Bind (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance monadInit :: Monad (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance monadTransInit :: MonadTrans (InitT info internal timerName timerContent state stateData)

instance supportsSelfInitT :: SupportsSelf InitT info internal timerName timerContent state stateData where
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
type StateEnterContext info internal timerName timerContent state stateData = {}

newtype StateEnterActionsBuilder timerName timerContent =
  StateEnterActionsBuilder (List (StateEnterAction timerName timerContent))

newtype StateEnterT info internal timerName timerContent state stateData m a =
  StateEnterT (StateT (StateEnterContext info internal timerName timerContent state stateData) m a)

derive newtype instance functorStateEnter :: Functor (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance applyStateEnter :: Apply (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance applicativeStateEnter :: Applicative (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance bindStateEnter :: Bind (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance monadStateEnter :: Monad (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance monadTransStateEnter :: MonadTrans (StateEnterT info internal timerName timerContent state stateData)

instance supportsSelfStateEnterT :: SupportsSelf StateEnterT info internal timerName timerContent state stateData where
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
type EventContext info internal timerName timerContent state stateData = {}

newtype EventActionsBuilder info internal timerName timerContent =
  EventActionsBuilder (List (EventAction info internal timerName timerContent))

newtype EventT info internal timerName timerContent state stateData m a =
  EventT (StateT (EventContext info internal timerName timerContent state stateData) m a)

derive newtype instance functorEvent :: Functor (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance applyEvent :: Apply (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance applicativeEvent :: Applicative (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance bindEvent :: Bind (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance monadEvent :: Monad (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance monadTransEvent :: MonadTrans (EventT info internal timerName timerContent state stateData)

instance supportsSelfEventT :: SupportsSelf EventT info internal timerName timerContent state stateData where
  self = EventT $ Exports.lift $ selfFFI

instance supportsNewActionsEventActionsBuilder :: SupportsNewActions (EventActionsBuilder info internal timerName timerContent) where
  newActions = EventActionsBuilder nil

instance supportsReplyEventActionsBuilder :: SupportsReply (EventActionsBuilder info internal timerName timerContent) where
  addReply reply (EventActionsBuilder actions) = EventActionsBuilder $ (CommonAction (ReplyAction reply)) : actions

-- -----------------------------------------------------------------------------
-- Other Gubbins
-- -----------------------------------------------------------------------------
newtype StatemType info internal timerName timerContent state stateData = StatemType Void

type Spec info internal timerName timerContent state stateData =
  { name :: Maybe (RegistryName (StatemType info internal timerName timerContent state stateData))
  , init :: InitFn info internal timerName timerContent state stateData
  , handleEvent :: EventFn info internal timerName timerContent state stateData
  , handleEnter :: Maybe (EnterFn info internal timerName timerContent state stateData)
  }

type InitFn info internal timerName timerContent state stateData =
  InitT info internal timerName timerContent state stateData Effect (InitResult info internal timerName timerContent state stateData)

type CallFn reply info internal timerName timerContent state stateData =
  From reply ->
  state ->
  stateData ->
  EventT info internal timerName timerContent state stateData Effect (CallResult info internal timerName timerContent state stateData)

type EventFn info internal timerName timerContent state stateData =
  (Event info internal timerName timerContent) ->
  state ->
  stateData ->
  EventT info internal timerName timerContent state stateData Effect (EventResult info internal timerName timerContent state stateData)

type EnterFn info internal timerName timerContent state stateData =
  state ->
  state ->
  stateData ->
  StateEnterT info internal timerName timerContent state stateData Effect (StateEnterResult timerName timerContent state stateData)

data CallResult info internal timerName timerContent state stateData
  = CallKeepState stateData
  | CallKeepStateWithActions stateData (EventActionsBuilder info internal timerName timerContent)
  | CallKeepStateAndData
  | CallKeepStateAndDataWithActions (EventActionsBuilder info internal timerName timerContent)

  | CallNextState state stateData
  | CallNextStateWithActions state stateData (EventActionsBuilder info internal timerName timerContent)

data EventResult info internal timerName timerContent state stateData
  = EventKeepState stateData
  | EventKeepStateWithActions stateData (EventActionsBuilder info internal timerName timerContent)
  | EventKeepStateAndData
  | EventKeepStateAndDataWithActions (EventActionsBuilder info internal timerName timerContent)

  | EventNextState state stateData
  | EventNextStateWithActions state stateData (EventActionsBuilder info internal timerName timerContent)

data StateEnterResult timerName timerContent state stateData
  = StateEnterKeepState stateData
  | StateEnterKeepStateWithActions (StateEnterActionsBuilder timerName timerContent) stateData
  | StateEnterKeepStateAndData
  | StateEnterKeepStateAndDataWithActions (StateEnterActionsBuilder timerName timerContent)

data InitResult info internal timerName timerContent state stateData
  = InitOk state stateData
  | InitOkWithActions state stateData (InitActionsBuilder info internal timerName timerContent)
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

foreign import data Reply :: Type
foreign import data FromForeign :: Type
newtype From reply = From FromForeign

foreign import mkReply :: forall reply. From reply -> reply -> Reply

startLink :: forall info internal timerName timerContent state stateData. Spec info internal timerName timerContent state stateData -> Effect (StartLinkResult (StatemType info internal timerName timerContent state stateData))
startLink _ = unsafeCoerce unit

mkSpec ::
  forall info internal timerName timerContent state stateData.
  InitFn info internal timerName timerContent state stateData ->
  EventFn info internal timerName timerContent state stateData ->
  Spec info internal timerName timerContent state stateData

mkSpec initFn handleEventFn =
  { name: Nothing
  , init: initFn
  , handleEvent: handleEventFn
  , handleEnter: Nothing
  }

call :: forall reply info internal timerName timerContent state stateData. InstanceRef (StatemType info internal timerName timerContent state stateData) -> CallFn reply info internal timerName timerContent state stateData -> Effect reply
call _instanceRef _callFn = unsafeCoerce unit
