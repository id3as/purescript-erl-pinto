module Pinto.GenStatem
       ( StatemType
       , StateEnterResult(..)
       , StateEnterAction
       , HandleEventResult(..)
       , EventAction(..)
       , CommonAction(..)
       , TimeoutAction(..)
       , Timeout(..)
       , Event(..)
       , From
       , Reply
       , mkReply
       , startLink
       , mkSpec
       , Running(..)
       , Spec
       , call
       )
       where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Data.List (List)
import Pinto.Types (RegistryName, ServerPid, StartLinkResult)
import Unsafe.Coerce (unsafeCoerce)

newtype StatemType state stateData = StatemType Void

type Spec state stateData =
  { name :: Maybe (RegistryName (StatemType state stateData))
  }

-- type InitFn state stateData = ResultT (InitResult state stateData) state stateData
-- type ResultT result state stateData = ReaderT (Context state stateData)  Effect result
--
-- newtype Context cont stop msg state
--   = Context
--     { handleInfo :: Maybe (WrappedInfoFn cont stop msg state)
--     , handleContinue :: Maybe (WrappedContinueFn cont stop msg state)
--     }

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

data Running state stateData
  = InitOk state stateData

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


startLink :: forall state stateData. Spec state stateData -> Effect (StartLinkResult (StatemType state stateData))
startLink _ = unsafeCoerce unit

mkSpec :: forall state stateData. Spec state stateData
mkSpec = unsafeCoerce unit

call _ _ = unsafeCoerce unit
