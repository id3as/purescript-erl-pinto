module Pinto.GenStatem
       ( StatemType
       , Context
       , ResultT

       , InitFn
       , InitResult

       , CallFn
       , HandleEventResult(..)

       , StateEnterResult(..)

       , StateEnterAction
       , EventAction(..)
       , CommonAction(..)
       , TimeoutAction(..)

       , Timeout(..)
       , Event(..)
       , Running(..)
       , NotRunning(..)
       , Spec

       , From
       , Reply
       , mkReply

       , startLink
       , mkSpec
       , call
       )
       where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.List (List)
import Foreign (Foreign)
import Pinto.Types (InstanceRef, RegistryName, ServerPid, StartLinkResult)
import Unsafe.Coerce (unsafeCoerce)

newtype StatemType info internal timerName timerContent state stateData = StatemType Void

type Spec info internal timerName timerContent state stateData =
  { name :: Maybe (RegistryName (StatemType info internal timerName timerContent state stateData))
  , init :: InitFn info internal timerName timerContent state stateData
  }

type InitFn info internal timerName timerContent state stateData = ResultT (InitResult info internal timerName timerContent state stateData) info internal timerName timerContent state stateData
type InitResult info internal timerName timerContent state stateData = Either NotRunning (Running info internal timerName timerContent state stateData)
type CallFn reply info internal timerName timerContent state stateData = From reply -> state -> stateData -> ResultT (HandleEventResult info internal timerName timerContent state stateData) info internal timerName timerContent state stateData

data Running info internal timerName timerContent state stateData
  = Init state stateData
  | InitWithActions state stateData (List (EventAction info internal timerName timerContent))
  | InitWithHibernate state

data NotRunning
  = InitStop Foreign
  | InitIgnore

type ResultT result info internal timerName timerContent state stateData = ReaderT (Context info internal timerName timerContent state stateData) Effect result

newtype Context info internal timerName timerContent state stateData =
  Context
  {
  }

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
  | TimeoutAction (TimeoutAction timerName timerContent)
  | ReplyAction Reply

data TimeoutAction timerName timerContent
  = SetTimeout (Timeout timerContent)
  | SetNamedTimeout timerName (Timeout timerContent)
  | SetStateTimeout (Timeout timerContent)
  | UpdateTimeout timerContent
  | UpdateNamedTimeout timerName timerContent
  | UpdateStateTimeout timerContent

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

mkSpec :: forall info internal timerName timerContent state stateData. InitFn info internal timerName timerContent state stateData -> Spec info internal timerName timerContent state stateData
mkSpec initFn =
  { name: Nothing
  , init: initFn
  }

call :: forall reply info internal timerName timerContent state stateData. InstanceRef (StatemType info internal timerName timerContent state stateData) -> CallFn reply info internal timerName timerContent state stateData -> Effect reply
call _instanceRef _callFn = unsafeCoerce unit
