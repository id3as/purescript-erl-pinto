module Pinto.GenServer
  ( InitFn
  , InitResult(..)
  , ServerSpec
  , ServerType
  , ServerPid
  , ServerRef(..)
  , CallFn
  , CallResult(..)
  , CastFn
  , InfoFn
  , ContinueFn
  , ReturnResult(..)
  , From
  , ResultT
  , Context
  , Action(..)
  , SimpleType
  , SimplePid
  , mkSpec
  , startLink
  , call
  , cast
  , stop
  , reply
  , replyWithAction
  , noReply
  , noReplyWithAction
  , return
  , returnWithAction
  , replyTo
  , self
  , module Exports
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (lift) as Exports
import Control.Monad.Reader as Reader
import Data.Function.Uncurried (Fn1, Fn2, mkFn1, mkFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign (Foreign)
import Pinto.Types (RegistryName, StartLinkResult, class HasRawPid, class HasProcess)
import Erl.Process (Process)

-- Sequence of types
-- reply cont stop msg [Timeout] state
--------------------------------------------------------------------------------
-- Public types
--------------------------------------------------------------------------------
data Action cont stop
  = Timeout Int
  | Hibernate
  | Continue cont
  | StopNormal
  | StopOther stop

data CallResult reply cont stop state
  = CallResult (Maybe reply) (Maybe (Action cont stop)) state

instance mapCallResult :: Functor (CallResult reply cont stop) where
  map f (CallResult mReply mAction state) = CallResult mReply mAction (f state)

data ReturnResult cont stop state
  = ReturnResult (Maybe (Action cont stop)) state

instance mapReturnResult :: Functor (ReturnResult cont stop) where
  map f (ReturnResult mAction state) = ReturnResult mAction (f state)

reply :: forall reply cont stop state. reply -> state -> CallResult reply cont stop state
reply theReply state = CallResult (Just theReply) Nothing state

replyWithAction :: forall reply cont stop state. reply -> Action cont stop -> state -> CallResult reply cont stop state
replyWithAction theReply action state = CallResult (Just theReply) (Just action) state

noReply :: forall reply cont stop state. state -> CallResult reply cont stop state
noReply state = CallResult Nothing Nothing state

noReplyWithAction :: forall reply cont stop state. Action cont stop -> state -> CallResult reply cont stop state
noReplyWithAction action state = CallResult Nothing (Just action) state

return :: forall cont stop state. state -> ReturnResult cont stop state
return state = ReturnResult Nothing state

returnWithAction :: forall cont stop state. Action cont stop -> state -> ReturnResult cont stop state
returnWithAction action state = ReturnResult (Just action) state

-- noReply...
-- return state
-- returnWithAction (Timeout 10) state
type ResultT result cont stop msg state
  = ReaderT (Context cont stop msg state) Effect result

foreign import data FromForeign :: Type

newtype From :: Type -> Type
newtype From reply
  = From FromForeign

-- reply cont stop msg [Timeout] state
-- TODO make order of type variables consistent
type InitFn cont stop msg state
  = ResultT (InitResult cont state) cont stop msg state

type CallFn reply cont stop msg state
  = From reply -> state -> ResultT (CallResult reply cont stop state) cont stop msg state

type CastFn cont stop msg state
  = state -> ResultT (ReturnResult cont stop state) cont stop msg state

type ContinueFn cont stop msg state
  = cont -> state -> ResultT (ReturnResult cont stop state) cont stop msg state

type InfoFn cont stop msg state
  = msg -> state -> ResultT (ReturnResult cont stop state) cont stop msg state

-- -- | Type of the callback invoked during a gen_server:handle_cast
-- type Cast state msg = ResultT (CastResult state) state msg
data InitResult cont state
  = InitOk state
  | InitOkTimeout state Int
  | InitOkContinue state cont
  | InitOkHibernate state
  | InitStop Foreign
  | InitIgnore

-- Can't do a functor instance over a type synonym, so just have a function instead
mapInitResult :: forall state state' cont. (state -> state') -> InitResult cont state -> InitResult cont state'
mapInitResult f (InitOk state) = InitOk $ f state

mapInitResult f (InitOkTimeout state timeout) = InitOkTimeout (f state) timeout

mapInitResult f (InitOkContinue state cont) = InitOkContinue (f state) cont

mapInitResult f (InitOkHibernate state) = InitOkHibernate $ f state

mapInitResult _ (InitStop term) = InitStop term

mapInitResult _ InitIgnore = InitIgnore

type SimpleType msg state
  = ServerType Unit Unit msg state

type SimplePid msg state
  = ServerPid Unit Unit msg state

newtype ServerType :: Type -> Type -> Type -> Type -> Type
newtype ServerType cont stop msg state
  = ServerType Void

newtype ServerPid :: Type -> Type -> Type -> Type -> Type
newtype ServerPid cont stop msg state
  = ServerPid (Process msg)

derive newtype instance serverPidHasRawPid :: HasRawPid (ServerPid cont stop msg state)

derive newtype instance serverPidHasProcess :: HasProcess msg (ServerPid const stop msg state)

data ServerRef cont stop msg state
  = ByName (RegistryName (ServerType cont stop msg state))
  | ByPid (ServerPid cont stop msg state)

type ServerSpec cont stop msg state
  = { name :: Maybe (RegistryName (ServerType cont stop msg state))
    , init :: InitFn cont stop msg state
    , handleInfo :: Maybe (InfoFn cont stop msg state)
    , handleContinue :: Maybe (ContinueFn cont stop msg state)
    }

--------------------------------------------------------------------------------
-- Internal types
--------------------------------------------------------------------------------
type OuterState cont stop msg state
  = { innerState :: state
    , context :: Context cont stop msg state
    }

mkOuterState :: forall cont stop msg state. Context cont stop msg state -> state -> OuterState cont stop msg state
mkOuterState context innerState = { context, innerState }

newtype Context cont stop msg state
  = Context
  { handleInfo :: Maybe (WrappedInfoFn cont stop msg state)
  , handleContinue :: Maybe (WrappedContinueFn cont stop msg state)
  }

type WrappedInfoFn cont stop msg state
  = Fn2 msg (OuterState cont stop msg state) (Effect (ReturnResult cont stop (OuterState cont stop msg state)))

type WrappedCallFn reply cont stop msg state
  = Fn2 (From reply) (OuterState cont stop msg state) (Effect (CallResult reply cont stop (OuterState cont stop msg state)))

type WrappedCastFn cont stop msg state
  = Fn1 (OuterState cont stop msg state) (Effect (ReturnResult cont stop (OuterState cont stop msg state)))

type WrappedContinueFn cont stop msg state
  = Fn2 cont (OuterState cont stop msg state) (Effect (ReturnResult cont stop (OuterState cont stop msg state)))

mkSpec :: forall cont stop msg state. InitFn cont stop msg state -> ServerSpec cont stop msg state
mkSpec initFn =
  { name: Nothing
  , init: initFn
  , handleInfo: Nothing
  , handleContinue: Nothing
  }

foreign import callFFI ::
  forall reply cont stop msg state.
  ServerRef cont stop msg state ->
  WrappedCallFn reply cont stop msg state ->
  Effect reply

call ::
  forall reply cont stop msg state.
  ServerRef cont stop msg state ->
  CallFn reply cont stop msg state ->
  Effect reply
call instanceRef callFn = callFFI instanceRef wrappedCallFn
  where
  wrappedCallFn :: WrappedCallFn reply cont stop msg state
  wrappedCallFn =
    let
      handler from state@{ innerState, context: handlerContext } = do
        innerResult <- (runReaderT $ callFn from innerState) handlerContext
        pure $ (mkOuterState handlerContext) <$> innerResult
    in
      mkFn2 handler

foreign import replyTo :: forall reply. From reply -> reply -> Effect Unit

foreign import castFFI ::
  forall cont stop msg state.
  ServerRef cont stop msg state ->
  WrappedCastFn cont stop msg state ->
  Effect Unit

cast ::
  forall cont stop msg state.
  ServerRef cont stop msg state ->
  CastFn cont stop msg state ->
  Effect Unit
cast instanceRef castFn = castFFI instanceRef wrappedCastFn
  where
  wrappedCastFn :: WrappedCastFn cont stop msg state
  wrappedCastFn =
    let
      handler state@{ innerState, context: handlerContext } = do
        innerResult <- (runReaderT $ castFn innerState) handlerContext
        pure $ (mkOuterState handlerContext) <$> innerResult
    in
      mkFn1 handler

-- TODO: should we transform the thrown noproc?
foreign import stopFFI ::
  forall cont stop msg state.
  ServerRef cont stop msg state ->
  Effect Unit

stop ::
  forall cont stop msg state.
  ServerRef cont stop msg state ->
  Effect Unit
stop = stopFFI

startLink :: forall cont stop msg state. (ServerSpec cont stop msg state) -> Effect (StartLinkResult (ServerPid cont stop msg state))
startLink { name: maybeName, init: initFn, handleInfo: maybeHandleInfo, handleContinue: maybeHandleContinue } = startLinkFFI maybeName initEffect
  where
  context =
    Context
      { handleInfo: wrapHandleInfo <$> maybeHandleInfo
      , handleContinue: wrapContinue <$> maybeHandleContinue
      }

  initEffect :: Effect (InitResult cont (OuterState cont stop msg state))
  initEffect = do
    innerResult <- (runReaderT initFn) context
    pure $ mapInitResult (mkOuterState context) innerResult

  wrapHandleInfo :: InfoFn cont stop msg state -> WrappedInfoFn cont stop msg state
  wrapHandleInfo handleInfo =
    let
      handler msg state@{ innerState, context: handlerContext } = do
        innerResult <- (runReaderT $ handleInfo msg innerState) handlerContext
        pure $ (mkOuterState handlerContext) <$> innerResult
    in
      mkFn2 handler

  wrapContinue :: ContinueFn cont stop msg state -> WrappedContinueFn cont stop msg state
  wrapContinue continueFn =
    let
      handler cont state@{ innerState, context: handlerContext } = do
        innerResult <- (runReaderT $ continueFn cont innerState) handlerContext
        pure $ (mkOuterState handlerContext) <$> innerResult
    in
      mkFn2 handler

foreign import startLinkFFI ::
  forall cont stop msg state.
  Maybe (RegistryName (ServerType cont stop msg state)) ->
  Effect (InitResult cont (OuterState cont stop msg state)) ->
  Effect (StartLinkResult (ServerPid cont stop msg state))

self ::
  forall cont stop msg state.
  ReaderT (Context cont stop msg state) Effect (ServerPid cont stop msg state)
self = Reader.lift selfFFI

foreign import selfFFI ::
  forall cont stop msg state.
  Effect (ServerPid cont stop msg state)
