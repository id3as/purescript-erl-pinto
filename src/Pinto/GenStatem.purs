module Pinto.GenStatem
       ( StatemType
       , StateEnterResult(..)
       , StateEnterAction(..)
       , TimeoutAction(..)
       , Timeout(..)
       , From
       , Reply
       , mkReply
       , startLink
       , mkSpec
       , Running(..)
       , Spec
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

data StateEnterResult timerName timerEventContent state stateData
  = KeepState stateData
  | KeepStateWithActions (List (StateEnterAction timerName timerEventContent)) stateData
  | KeepStateAndData
  | KeepStateAndDataWithActions (List (StateEnterAction timerName timerEventContent))

data Running state stateData
  = InitOk state stateData

data StateEnterAction timerName timerEventContent
  = Hibernate
  | TimeoutAction (TimeoutAction timerName timerEventContent)
  | ReplyAction Reply

data TimeoutAction timerName timerEventContent
  = SetTimeout (Timeout timerEventContent)
  | SetNamedTimeout timerName (Timeout timerEventContent)
  | SetStateTimeout (Timeout timerEventContent)
  | UpdateTimeout timerEventContent
  | UpdateNamedTimeout timerName timerEventContent
  | UpdateStateTimeout timerEventContent

data Timeout timerEventContent
  = At Int (Maybe timerEventContent)
  | After Int (Maybe timerEventContent)
  | Cancel

foreign import data Reply :: Type
foreign import data FromForeign :: Type
newtype From reply = From FromForeign

foreign import mkReply :: forall reply. From reply -> reply -> Reply


startLink :: forall state stateData. Spec state stateData -> Effect (StartLinkResult (StatemType state stateData))
startLink _ = unsafeCoerce unit

mkSpec :: forall state stateData. Spec state stateData
mkSpec = unsafeCoerce unit
