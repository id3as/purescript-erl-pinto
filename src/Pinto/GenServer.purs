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
  , ExitMessage(..)
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
  , module Exports
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (lift) as Exports
import Control.Monad.Reader as Reader
import Data.Maybe (Maybe(..), fromJust, fromMaybe')
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3)
import Erl.Atom (atom)
import Erl.Data.List (List, head)
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Erl.ModuleName (NativeModuleName, nativeModuleName)
import Erl.Process (class HasProcess, Process)
import Erl.Process.Raw (class HasPid, Pid, setProcessFlagTrapExit)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Pinto.ModuleNames (pintoGenServer)
import Pinto.Types (RegistryInstance, RegistryName, RegistryReference, StartLinkResult, registryInstance, class ExportsTo, export)
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

-- | A trapped exit
data ExitMessage
  = Exit Pid Foreign

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
    , trapExits :: Maybe (ExitMessage -> msg)
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
  , trapExits :: Maybe (ExitMessage -> msg)
  }

defaultSpec :: forall cont stop msg state. InitFn cont stop msg state -> ServerSpec cont stop msg state
defaultSpec initFn =
  { name: Nothing
  , init: initFn
  , handleInfo: Nothing
  , handleContinue: Nothing
  , trapExits: Nothing
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
startLink { name: maybeName, init: initFn, handleInfo: maybeHandleInfo, handleContinue: maybeHandleContinue, trapExits: maybeTrapExits } = startLinkFFI maybeName (nativeModuleName pintoGenServer) initEffect
  where
  context =
    Context
      { handleInfo: maybeHandleInfo
      , handleContinue: maybeHandleContinue
      , trapExits: maybeTrapExits
      }

  initEffect :: Effect (InitResult cont (OuterState cont stop msg state))
  initEffect = do
    _ <- case maybeTrapExits of
      Nothing -> pure unit
      Just _ -> void $ setProcessFlagTrapExit true
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
    export <$> impl

handle_call :: forall reply cont stop msg state. EffectFn3 (CallFn reply cont stop msg state) (From reply) (OuterState cont stop msg state) (NativeCallResult reply cont stop (OuterState cont stop msg state))
handle_call =
  mkEffectFn3 \f from state@{ innerState, context } -> do
    result <- (runReaderT $ f from innerState) context
    pure $ export (mkOuterState context <$> result)

handle_cast :: forall cont stop msg state. EffectFn2 (CastFn cont stop msg state) (OuterState cont stop msg state) (NativeReturnResult cont stop (OuterState cont stop msg state))
handle_cast =
  mkEffectFn2 \f state@{ innerState, context } -> do
    result <- (runReaderT $ f innerState) context
    pure $ export (mkOuterState context <$> result)

handle_info :: forall cont stop msg state. EffectFn2 Foreign (OuterState cont stop msg state) (NativeReturnResult cont stop (OuterState cont stop msg state))
handle_info =
  mkEffectFn2 \nativeMsg state@{ innerState, context: context@(Context { handleInfo: maybeHandleInfo, trapExits }) } ->
    let
      exitMessage = parseTrappedExitFFI nativeMsg Exit

      msg :: msg
      msg = fromMaybe' (\_ -> assumeExpectedMessage nativeMsg) $ trapExits <*> exitMessage
    in
      export
        <$> case maybeHandleInfo of
            Just f -> do
              result <- (runReaderT $ f msg innerState) context
              pure $ (mkOuterState context <$> result)
            Nothing -> pure $ ReturnResult Nothing state

foreign import parseTrappedExitFFI :: Foreign -> (Pid -> Foreign -> ExitMessage) -> Maybe ExitMessage

assumeExpectedMessage :: forall msg. Foreign -> msg
assumeExpectedMessage = unsafeCoerce

handle_continue :: forall cont stop msg state. EffectFn2 cont (OuterState cont stop msg state) (NativeReturnResult cont stop (OuterState cont stop msg state))
handle_continue =
  mkEffectFn2 \msg state@{ innerState, context: context@(Context { handleContinue: maybeHandleContinue }) } ->
    export
      <$> case maybeHandleContinue of
          Just f -> do
            result <- (runReaderT $ f msg innerState) context
            pure $ (mkOuterState context <$> result)
          Nothing -> pure $ ReturnResult Nothing state

instance exportInitResult :: ExportsTo (InitResult cont state) (NativeInitResult state) where
  export = case _ of
    InitStop err -> unsafeCoerce $ tuple2 (atom "stop") err
    InitIgnore -> unsafeCoerce $ atom "ignore"
    InitOk state -> unsafeCoerce $ tuple2 (atom "ok") state
    InitOkTimeout state timeout -> unsafeCoerce $ tuple3 (atom "timeout") state timeout
    InitOkContinue state cont -> unsafeCoerce $ tuple3 (atom "ok") state $ tuple2 (atom "continue") cont
    InitOkHibernate state -> unsafeCoerce $ tuple3 (atom "ok") state (atom "hibernate")

instance exportCallResult :: ExportsTo (CallResult reply cont stop outerState) (NativeCallResult reply cont stop outerState) where
  export = case _ of
    CallResult (Just r) Nothing newState -> unsafeCoerce $ tuple3 (atom "reply") r newState
    CallResult (Just r) (Just (Timeout timeout)) newState -> unsafeCoerce $ tuple4 (atom "reply") r newState timeout
    CallResult (Just r) (Just Hibernate) newState -> unsafeCoerce $ tuple4 (atom "reply") r newState (atom "hibernate")
    CallResult (Just r) (Just (Continue cont)) newState -> unsafeCoerce $ tuple4 (atom "reply") r newState $ tuple2 (atom "continue") cont
    CallResult (Just r) (Just StopNormal) newState -> unsafeCoerce $ tuple4 (atom "stop") (atom "normal") r newState
    CallResult (Just r) (Just (StopOther reason)) newState -> unsafeCoerce $ tuple4 (atom "stop") reason r newState
    CallResult Nothing Nothing newState -> unsafeCoerce $ tuple2 (atom "reply") newState
    CallResult Nothing (Just (Timeout timeout)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState timeout
    CallResult Nothing (Just Hibernate) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState (atom "hibernate")
    CallResult Nothing (Just (Continue cont)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ tuple2 (atom "continue") cont
    CallResult Nothing (Just StopNormal) newState -> unsafeCoerce $ tuple3 (atom "stop") (atom "normal") newState
    CallResult Nothing (Just (StopOther reason)) newState -> unsafeCoerce $ tuple3 (atom "stop") reason newState

instance exportReturnResult :: ExportsTo (ReturnResult cont stop outerState) (NativeReturnResult cont stop outerState) where
  export = case _ of
    ReturnResult Nothing newState -> unsafeCoerce $ tuple2 (atom "noreply") newState
    ReturnResult (Just (Timeout timeout)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState timeout
    ReturnResult (Just Hibernate) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ atom "hibernate"
    ReturnResult (Just (Continue cont)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ tuple2 (atom "continue") cont
    ReturnResult (Just StopNormal) newState -> unsafeCoerce $ tuple3 (atom "stop") (atom "normal") newState
    ReturnResult (Just (StopOther reason)) newState -> unsafeCoerce $ tuple3 (atom "stop") reason newState

foreign import data NativeInitResult :: Type -> Type

foreign import data NativeCallResult :: Type -> Type -> Type -> Type -> Type

foreign import data NativeReturnResult :: Type -> Type -> Type -> Type
