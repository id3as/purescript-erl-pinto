module Pinto.GenStatem
       ( StatemType

       , class SupportsReply
       , addReply

       , class SupportsAddTimeout
       , addTimeoutAction

       , class SupportsChangeState
       , changeState

       , class SupportsChangeStateData
       , changeStateData

       , class SupportsSelf
       , self

       , InitFn
       , InitResult(..)
       , InitT

       , CallFn
       , EventFn
       , EventT

       , EnterFn
       , StateEnterT

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
import Control.Monad.State as StateM
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Trans.Class (lift) as Exports

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (List, (:))
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
class SupportsReply m where
  addReply :: Reply -> m Unit

class SupportsAddTimeout m timerContent  where
  addTimeoutAction :: TimeoutAction timerContent -> m Unit

class SupportsChangeState m state where
  changeState :: state -> m Unit

class SupportsChangeStateData m stateData where
  changeStateData :: stateData -> m Unit

class SupportsSelf m info internal timerName timerContent state stateData where
  self :: m info internal timerName timerContent state stateData Effect (ServerPid (StatemType info internal timerName timerContent state stateData))

-- -----------------------------------------------------------------------------
-- InitT
-- -----------------------------------------------------------------------------
type InitResponseBuilder info internal timerName timerContent state stateData =
  { actions :: List (EventAction info internal timerName timerContent)
  }

newtype InitT info internal timerName timerContent state stateData m a =
  InitT (StateT (InitResponseBuilder info internal timerName timerContent state stateData) m a)

derive newtype instance functorInit :: Functor (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance applyInit :: Apply (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance applicativeInit :: Applicative (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance bindInit :: Bind (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance monadInit :: Monad (InitT info internal timerName timerContent state stateData Effect)
derive newtype instance monadTransInit :: MonadTrans (InitT info internal timerName timerContent state stateData)

instance supportsReplyInitT :: SupportsReply (InitT info internal timerName timerContent state stateData Effect) where
  addReply reply = InitT (StateM.modify_ (\context@{ actions } -> context { actions = (CommonAction $ ReplyAction reply) : actions }))

instance supportsAddTimeoutInitT :: SupportsAddTimeout (InitT info internal timerName timerContent state stateData Effect) timerContent where
  addTimeoutAction action = InitT (StateM.modify_ (\context@{ actions } -> context { actions = (CommonAction $ TimeoutAction action) : actions }))

instance supportsSelfInitT :: SupportsSelf InitT info internal timerName timerContent state stateData where
  self = InitT $ Exports.lift $ selfFFI

-- -----------------------------------------------------------------------------
-- StateEnterT
-- -----------------------------------------------------------------------------
-- TODO: we've not yet modeled things like stop/stop_and_reply, they are a bit interesting
-- because they only admit replies, not other sorts of actions, so do we simply throw away
-- any actions we've accumulated aside from the replies, or throw them all and have a function which is
-- specifically for stop_and_reply? or something else
type StateEnterResponseBuilder info internal timerName timerContent state stateData =
  { actions :: List (StateEnterAction timerName timerContent)
  , newState :: Maybe state
  , newData :: Maybe stateData
  }

newtype StateEnterT info internal timerName timerContent state stateData m a =
  StateEnterT (StateT (StateEnterResponseBuilder info internal timerName timerContent state stateData) m a)

derive newtype instance functorStateEnter :: Functor (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance applyStateEnter :: Apply (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance applicativeStateEnter :: Applicative (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance bindStateEnter :: Bind (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance monadStateEnter :: Monad (StateEnterT info internal timerName timerContent state stateData Effect)
derive newtype instance monadTransStateEnter :: MonadTrans (StateEnterT info internal timerName timerContent state stateData)

instance supportsReplyStateEnterT :: SupportsReply (StateEnterT info internal timerName timerContent state stateData Effect) where
  addReply reply = StateEnterT (StateM.modify_ (\context@{ actions } -> context { actions = ReplyAction reply : actions }))

instance supportsAddTimeoutStateEnterT :: SupportsAddTimeout (StateEnterT info internal timerName timerContent state stateData Effect) timerContent where
  addTimeoutAction action = StateEnterT (StateM.modify_ (\context@{ actions } -> context { actions = TimeoutAction action : actions }))

instance supportsChangeStateStateEnterT :: SupportsChangeState (StateEnterT info internal timerName timerContent state stateData Effect) state where
  changeState state = StateEnterT (StateM.modify_ (\context@{ actions } -> context { newState = Just state }))

instance supportsChangeStateDataStateEnterT :: SupportsChangeStateData (StateEnterT info internal timerName timerContent state stateData Effect) stateData where
  changeStateData stateData = StateEnterT (StateM.modify_ (\context@{ actions } -> context { newData = Just stateData }))

instance supportsSelfStateEnterT :: SupportsSelf StateEnterT info internal timerName timerContent state stateData where
  self = StateEnterT $ Exports.lift $ selfFFI

-- -----------------------------------------------------------------------------
-- EventT
-- -----------------------------------------------------------------------------
type EventResponseBuilder info internal timerName timerContent state stateData =
  { actions :: List (EventAction info internal timerName timerContent)
  , newState :: Maybe state
  , newData :: Maybe stateData
  }

newtype EventT info internal timerName timerContent state stateData m a =
  EventT (StateT (EventResponseBuilder info internal timerName timerContent state stateData) m a)

derive newtype instance functorEvent :: Functor (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance applyEvent :: Apply (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance applicativeEvent :: Applicative (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance bindEvent :: Bind (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance monadEvent :: Monad (EventT info internal timerName timerContent state stateData Effect)
derive newtype instance monadTransEvent :: MonadTrans (EventT info internal timerName timerContent state stateData)

instance supportsReplyEventT :: SupportsReply (EventT info internal timerName timerContent state stateData Effect) where
  addReply reply = EventT (StateM.modify_ (\context@{ actions } -> context { actions = (CommonAction (ReplyAction reply)) : actions }))

instance supportsChangeStateEventT :: SupportsChangeState (EventT info internal timerName timerContent state stateData Effect) state where
  changeState state = EventT (StateM.modify_ (\context@{ actions } -> context { newState = Just state }))

instance supportsChangeStateDataEventT :: SupportsChangeStateData (EventT info internal timerName timerContent state stateData Effect) stateData where
  changeStateData stateData = EventT (StateM.modify_ (\context@{ actions } -> context { newData = Just stateData }))

instance supportsSelfEventT :: SupportsSelf EventT info internal timerName timerContent state stateData where
  self = EventT $ Exports.lift $ selfFFI

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

type InitFn info internal timerName timerContent state stateData = InitT info internal timerName timerContent state stateData Effect (InitResult state stateData)
type CallFn reply info internal timerName timerContent state stateData = From reply -> state -> stateData -> EventT info internal timerName timerContent state stateData Effect Unit
type EventFn info internal timerName timerContent state stateData = (Event info internal timerName timerContent) -> state -> stateData -> EventT info internal timerName timerContent state stateData Effect Unit
type EnterFn info internal timerName timerContent state stateData = state -> state -> stateData -> StateEnterT info internal timerName timerContent state stateData Effect Unit

data InitResult state stateData
  = Init state stateData
  | InitWithHibernate state
  | InitStop Foreign
  | InitIgnore

data StateEnterResult timerName timerContent state stateData
  = StateEnterKeepState stateData
  | StateEnterKeepStateWithActions (List (StateEnterAction timerName timerContent)) stateData
  | StateEnterKeepStateAndData
  | StateEnterKeepStateAndDataWithActions (List (StateEnterAction timerName timerContent))

type StateEnterAction timerName timerContent = CommonAction timerName timerContent

data HandleEventResult info internal timerName timerContent state stateData
  = HandleEventKeepState stateData
  | HandleEventKeepStateWithActions (List (EventAction info internal timerName timerContent)) stateData
  | HandleEventKeepStateAndData
  | HandleEventKeepStateAndDataWithActions (List (EventAction info internal timerName timerContent))

  | HandleEventNextState state stateData
  | HandleEventNextStateWithActions state stateData (List (EventAction info internal timerName timerContent))

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
