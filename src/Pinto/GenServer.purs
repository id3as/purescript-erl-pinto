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
  , defaultSpec
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
  -- These probably need to go in a different module
  , init
  , handle_call
  , handle_cast
  , handle_info
  , handle_continue
  , NativeInitResult
  , NativeCallResult
  , NativeReturnResult
  , class ToNative
  , toNative
  , module Exports
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (lift) as Exports
import Control.Monad.Reader as Reader
import Data.Function.Uncurried (Fn1, Fn2, mkFn1, mkFn2)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3)
import Erl.Atom (atom)
import Erl.Data.List (List, head)
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Erl.ModuleName (NativeModuleName(..), nativeModuleName)
import Pinto.ModuleNames (pintoGenServer)
import Erl.Process (Process, class HasProcess)
import Erl.Process.Raw (class HasPid)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Pinto.Types (RegistryInstance, RegistryName, RegistryReference, StartLinkResult, registryInstance)
import Unsafe.Coerce (unsafeCoerce)

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

newtype ServerType :: Type -> Type -> Type -> Type -> Type
newtype ServerType cont stop msg state
  = ServerType Void

newtype ServerPid :: Type -> Type -> Type -> Type -> Type
newtype ServerPid cont stop msg state
  = ServerPid (Process msg)

derive newtype instance serverPidHasRawPid :: HasPid (ServerPid cont stop msg state)

derive newtype instance serverPidHasProcess :: HasProcess msg (ServerPid const stop msg state)

type ServerRef cont stop msg state
  = RegistryReference (ServerPid cont stop msg state) (ServerType cont stop msg state)

type ServerInstance cont stop msg state
  = RegistryInstance (ServerPid cont stop msg state) (ServerType cont stop msg state)

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
mkOuterState = { context: _, innerState: _ }

newtype Context cont stop msg state
  = Context
  { handleInfo :: Maybe (InfoFn cont stop msg state)
  , handleContinue :: Maybe (ContinueFn cont stop msg state)
  }

defaultSpec :: forall cont stop msg state. InitFn cont stop msg state -> ServerSpec cont stop msg state
defaultSpec initFn =
  { name: Nothing
  , init: initFn
  , handleInfo: Nothing
  , handleContinue: Nothing
  }

foreign import callFFI ::
  forall reply cont stop msg state.
  ServerInstance cont stop msg state ->
  CallFn reply cont stop msg state ->
  Effect reply

call ::
  forall reply cont stop msg state.
  ServerRef cont stop msg state ->
  CallFn reply cont stop msg state ->
  Effect reply
call r callFn = callFFI (registryInstance r) callFn

foreign import replyTo :: forall reply. From reply -> reply -> Effect Unit

foreign import castFFI ::
  forall cont stop msg state.
  ServerInstance cont stop msg state ->
  CastFn cont stop msg state ->
  Effect Unit

cast ::
  forall cont stop msg state.
  ServerRef cont stop msg state ->
  CastFn cont stop msg state ->
  Effect Unit
cast r castFn = castFFI (registryInstance r) castFn

foreign import stopFFI ::
  forall cont stop msg state.
  ServerInstance cont stop msg state ->
  Effect Unit

stop ::
  forall cont stop msg state.
  ServerRef cont stop msg state ->
  Effect Unit
stop r = stopFFI $ registryInstance r

startLink :: forall cont stop msg state. (ServerSpec cont stop msg state) -> Effect (StartLinkResult (ServerPid cont stop msg state))
startLink { name: maybeName, init: initFn, handleInfo: maybeHandleInfo, handleContinue: maybeHandleContinue } = startLinkFFI maybeName (nativeModuleName pintoGenServer) initEffect
  where
  context =
    Context
      { handleInfo: maybeHandleInfo
      , handleContinue: maybeHandleContinue
      }

  initEffect :: Effect (InitResult cont (OuterState cont stop msg state))
  initEffect = do
    innerResult <- (runReaderT initFn) context
    pure $ mapInitResult (mkOuterState context) innerResult

foreign import startLinkFFI ::
  forall cont stop msg state.
  Maybe (RegistryName (ServerType cont stop msg state)) ->
  NativeModuleName ->
  Effect (InitResult cont (OuterState cont stop msg state)) ->
  Effect (StartLinkResult (ServerPid cont stop msg state))

self ::
  forall cont stop msg state.
  ReaderT (Context cont stop msg state) Effect (ServerPid cont stop msg state)
self = Reader.lift selfFFI

foreign import selfFFI ::
  forall cont stop msg state.
  Effect (ServerPid cont stop msg state)

init ::
  forall cont state.
  EffectFn1 (List (Effect (InitResult cont state))) (NativeInitResult state)
init =
  mkEffectFn1 \args -> do
    let
      impl = unsafePartial $ fromJust $ head args
    toNative <$> impl

handle_call :: forall reply cont stop msg state. EffectFn3 (CallFn reply cont stop msg state) (From reply) (OuterState cont stop msg state) (NativeCallResult reply cont stop (OuterState cont stop msg state))
handle_call =
  mkEffectFn3 \f from state@{ innerState, context } -> do
    result <- (runReaderT $ f from innerState) context
    pure $ toNative (mkOuterState context <$> result)

handle_cast :: forall cont stop msg state. EffectFn2 (CastFn cont stop msg state) (OuterState cont stop msg state) (NativeReturnResult cont stop (OuterState cont stop msg state))
handle_cast =
  mkEffectFn2 \f state@{ innerState, context } -> do
    result <- (runReaderT $ f innerState) context
    pure $ toNative (mkOuterState context <$> result)

handle_info :: forall cont stop msg state. EffectFn2 msg (OuterState cont stop msg state) (NativeReturnResult cont stop (OuterState cont stop msg state))
handle_info =
  mkEffectFn2 \msg state@{ innerState, context: context@(Context { handleInfo: maybeHandleInfo }) } ->
    toNative
      <$> case maybeHandleInfo of
          Just f -> do
            result <- (runReaderT $ f msg innerState) context
            pure $ (mkOuterState context <$> result)
          Nothing -> pure $ ReturnResult Nothing state

handle_continue :: forall cont stop msg state. EffectFn2 cont (OuterState cont stop msg state) (NativeReturnResult cont stop (OuterState cont stop msg state))
handle_continue =
  mkEffectFn2 \msg state@{ innerState, context: context@(Context { handleContinue: maybeHandleContinue }) } ->
    toNative
      <$> case maybeHandleContinue of
          Just f -> do
            result <- (runReaderT $ f msg innerState) context
            pure $ (mkOuterState context <$> result)
          Nothing -> pure $ ReturnResult Nothing state

class ToNative a b where
  toNative :: a -> b

instance toNativeInitResult :: ToNative (InitResult cont state) (NativeInitResult state) where
  toNative = case _ of
    InitStop err -> unsafeCoerce $ tuple2 (atom "stop") err
    InitIgnore -> unsafeCoerce $ atom "ignore"
    InitOk state -> unsafeCoerce $ tuple2 (atom "ok") state
    InitOkTimeout state timeout -> unsafeCoerce $ tuple3 (atom "timeout") state timeout
    InitOkContinue state cont -> unsafeCoerce $ tuple3 (atom "ok") state $ tuple2 (atom "continue") cont
    InitOkHibernate state -> unsafeCoerce $ tuple3 (atom "ok") state (atom "hibernate")

instance toNativeCallResult :: ToNative (CallResult reply cont stop outerState) (NativeCallResult reply cont stop outerState) where
  toNative = case _ of
    CallResult (Just reply) Nothing newState -> unsafeCoerce $ tuple3 (atom "reply") reply newState
    CallResult (Just reply) (Just (Timeout timeout)) newState -> unsafeCoerce $ tuple4 (atom "reply") reply newState timeout
    CallResult (Just reply) (Just Hibernate) newState -> unsafeCoerce $ tuple4 (atom "reply") reply newState (atom "hibernate")
    CallResult (Just reply) (Just (Continue cont)) newState -> unsafeCoerce $ tuple4 (atom "reply") reply newState $ tuple2 (atom "continue") cont
    CallResult (Just reply) (Just StopNormal) newState -> unsafeCoerce $ tuple4 (atom "stop") (atom "normal") reply newState
    CallResult (Just reply) (Just (StopOther reason)) newState -> unsafeCoerce $ tuple4 (atom "stop") reason reply newState
    CallResult Nothing Nothing newState -> unsafeCoerce $ tuple2 (atom "reply") newState
    CallResult Nothing (Just (Timeout timeout)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState timeout
    CallResult Nothing (Just Hibernate) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState (atom "hibernate")
    CallResult Nothing (Just (Continue cont)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ tuple2 (atom "continue") cont
    CallResult Nothing (Just StopNormal) newState -> unsafeCoerce $ tuple3 (atom "stop") (atom "normal") newState
    CallResult Nothing (Just (StopOther reason)) newState -> unsafeCoerce $ tuple3 (atom "stop") reason newState

instance toNativeReturnResult :: ToNative (ReturnResult cont stop outerState) (NativeReturnResult cont stop outerState) where
  toNative = case _ of
    ReturnResult Nothing newState -> unsafeCoerce $ tuple2 (atom "noreply") newState
    ReturnResult (Just (Timeout timeout)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState timeout
    ReturnResult (Just Hibernate) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ atom "hibernate"
    ReturnResult (Just (Continue cont)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ tuple2 (atom "continue") cont
    ReturnResult (Just StopNormal) newState -> unsafeCoerce $ tuple3 (atom "stop") (atom "normal") newState
    ReturnResult (Just (StopOther reason)) newState -> unsafeCoerce $ tuple3 (atom "stop") reason newState

foreign import data NativeInitResult :: Type -> Type

foreign import data NativeCallResult :: Type -> Type -> Type -> Type -> Type

foreign import data NativeReturnResult :: Type -> Type -> Type -> Type
