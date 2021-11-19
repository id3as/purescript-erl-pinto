-- | Module representing the gen_server in OTP
-- | See also 'gen_server' in the OTP docs (https://erlang.org/doc/man/gen_server.html)
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
  , TerminateFn
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
  , whereIs
  , module ReExports
  , module Lift
  -- These probably need to go in a different module
  , init
  , handle_call
  , handle_cast
  , handle_info
  , handle_continue
  , terminate
  , NativeInitResult
  , NativeCallResult
  , NativeReturnResult
  -- these are only exported to get uncurried versions in the erlang
  , exportInitResult
  , exportCallResult
  , exportReturnResult
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Function.Uncurried (mkFn2, runFn2)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class (liftEffect) as Lift
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, head)
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Erl.ModuleName (NativeModuleName, nativeModuleName)
import Erl.Process (class HasProcess, getProcess, class HasSelf, class ReceivesMessage, Process)
import Erl.Process.Raw (class HasPid, setProcessFlagTrapExit)
import Foreign (Foreign, unsafeFromForeign)
import Partial.Unsafe (unsafePartial)
import Pinto.ModuleNames (pintoGenServer)
import Pinto.Types (ExitMessage(..), RegistryInstance, RegistryName, RegistryReference, ShutdownReason, StartLinkResult, parseShutdownReasonFFI, parseTrappedExitFFI, registryInstance)
import Pinto.Types (ShutdownReason(..), ExitMessage(..)) as ReExports
import Unsafe.Coerce (unsafeCoerce)

-- | The reader monad in which all GenServer operations take place
-- |
-- | - `cont` is the type that will be passed into a handle_continue callback,
-- |   if there is no handleContinue present, this can just be 'Unit'
-- | - `stop` is the data type that can be returned with the StopOther action
-- |   if StopOther is not being used, then this can simply 'Unit'
-- | - `msg` represents the type of message that this gen server will receive in its
-- |   handleInfo callback, if no messages are expected, this can simply be 'Unit'
-- | - `state` represents the internal state of this GenServer, created in 'init
-- |   and then passed into each subsequent callback
-- | - `result` is the result of any operation within a ResultT context
newtype ResultT cont stop msg state result
  = ResultT (ReaderT (Context cont stop msg state) Effect result)

derive newtype instance functorResultT :: Functor (ResultT cont stop msg state)
derive newtype instance applyResultT :: Apply (ResultT cont stop msg state)
derive newtype instance applicativeResultT :: Applicative (ResultT cont stop msg state)
derive newtype instance bindResultT :: Bind (ResultT cont stop msg state)
derive newtype instance monadResultT :: Monad (ResultT cont stop msg state)
derive newtype instance monadEffectResultT :: MonadEffect (ResultT cont stop msg state)
instance messageTypeResult :: ReceivesMessage (ResultT cont stop msg state) msg

-- | An action to be returned to OTP
-- | See {shutdown, reason}, {timeout...} etc in the gen_server documentation
-- | This should be constructed and returned with the xxWithAction methods inside GenServer callbacks
data Action cont stop
  = Timeout Int
  | Hibernate
  | Continue cont
  | StopNormal
  | StopOther stop

-- | The result of a GenServer.call (handle_call) action
data CallResult reply cont stop state
  = CallResult (Maybe reply) (Maybe (Action cont stop)) state

instance mapCallResult :: Functor (CallResult reply cont stop) where
  map f (CallResult mReply mAction state) = CallResult mReply mAction (f state)

-- | The result of a GenServer.handle_info or GenServer.handle_cast callback
data ReturnResult cont stop state
  = ReturnResult (Maybe (Action cont stop)) state

instance mapReturnResult :: Functor (ReturnResult cont stop) where
  map f (ReturnResult mAction state) = ReturnResult mAction (f state)

-- | Creates a result from inside a GenServer 'handle_call' that results in
-- | the 'reply' result being sent to the caller and the new state being stored
reply :: forall reply cont stop state. reply -> state -> CallResult reply cont stop state
reply theReply state = CallResult (Just theReply) Nothing state

-- | Creates a result from inside a GenServer 'handle_call' that results in
-- | the 'reply' result being sent to the caller , the new state being stored
-- | and the attached action being returned to OTP for processing
replyWithAction :: forall reply cont stop state. reply -> Action cont stop -> state -> CallResult reply cont stop state
replyWithAction theReply action state = CallResult (Just theReply) (Just action) state

-- | Creates a result from inside a GenServer 'handle_call' that results in
-- | the new state being stored and nothing being returned to the caller (yet)
noReply :: forall reply cont stop state. state -> CallResult reply cont stop state
noReply state = CallResult Nothing Nothing state

-- | Creates a result from inside a GenServer 'handle_call' that results in
-- | the new state being stored and nothing being returned to the caller (yet)
-- | and the attached action being returned to OTP for processing
noReplyWithAction :: forall reply cont stop state. Action cont stop -> state -> CallResult reply cont stop state
noReplyWithAction action state = CallResult Nothing (Just action) state

-- | Creates a result from inside a GenServer 'handle_info/handle_cast' that results in
-- | the new state being stored
return :: forall cont stop state. state -> ReturnResult cont stop state
return state = ReturnResult Nothing state

-- | Creates a result from inside a GenServer 'handle_info/handle_cast' that results in
-- | the new state being stored and the attached action being returned to OTP for processing
returnWithAction :: forall cont stop state. Action cont stop -> state -> ReturnResult cont stop state
returnWithAction action state = ReturnResult (Just action) state

foreign import data FromForeign :: Type

newtype From :: Type -> Type
newtype From reply
  = From FromForeign

-- | The callback invoked on GenServer startup: see gen_server:init
type InitFn cont stop msg state
  = ResultT cont stop msg state (InitResult cont state)

-- | The callback invoked within a GenServer.call: see gen_server:call
type CallFn reply cont stop msg state
  = From reply -> state -> ResultT cont stop msg state (CallResult reply cont stop state)

-- | The type of the handleCast callback see gen_server:cast
type CastFn cont stop msg state
  = state -> ResultT cont stop msg state (ReturnResult cont stop state)

-- | The type of the handleContinue callback see gen_server:handle_continue
type ContinueFn cont stop msg state
  = cont -> state -> ResultT cont stop msg state (ReturnResult cont stop state)

-- | The type of the handleInfo callback see gen_server:handle_info
type InfoFn cont stop msg state
  = msg -> state -> ResultT cont stop msg state (ReturnResult cont stop state)

-- | The type of the terminate callback see gen_server:terminate
type TerminateFn cont stop msg state
  = ShutdownReason -> state -> ResultT cont stop msg state Unit

-- | The various return values from an init callback
-- | These roughly map onto the tuples in the OTP documentation
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

-- | The typed reference of a GenServer, containing all the information required to get hold of
-- | an instance
type ServerRef cont stop msg state
  = RegistryReference (ServerPid cont stop msg state) (ServerType cont stop msg state)

-- | The typed instance of a GenServer, containing all the information required to call into
-- | a GenServer
type ServerInstance cont stop msg state
  = RegistryInstance (ServerPid cont stop msg state) (ServerType cont stop msg state)

-- | Given a RegistryName with a valid (ServerType), get hold of a typed Process `msg` to which messages
-- | can be sent (arriving in the handleInfo callback)
whereIs :: forall cont stop msg state. RegistryName (ServerType cont stop msg state) -> Effect (Maybe (Process msg))
whereIs _name =
  pure Nothing -- TODO: implement

-- | The configuration passed into startLink in order to start a gen server
-- | Everything except the 'init' callback is optional
-- | Note: GenServers started without a name will not be callable without some means
-- | of retrieving the pid
type ServerSpec cont stop msg state
  = { name :: Maybe (RegistryName (ServerType cont stop msg state))
    , init :: InitFn cont stop msg state
    , handleInfo :: Maybe (InfoFn cont stop msg state)
    , handleContinue :: Maybe (ContinueFn cont stop msg state)
    , terminate :: Maybe (TerminateFn cont stop msg state)
    , trapExits :: Maybe (ExitMessage -> msg)
    }

-- | Given an InitFn callback, create a default GenServer specification with all of the optionals
-- | set to default values
-- | This is the preferred method of creating the config passed into GenServer.startLink
defaultSpec :: forall cont stop msg state. InitFn cont stop msg state -> ServerSpec cont stop msg state
defaultSpec initFn =
  { name: Nothing
  , init: initFn
  , handleInfo: Nothing
  , handleContinue: Nothing
  , terminate: Nothing
  , trapExits: Nothing
  }

-- | Given a specification, starts a GenServer
-- |
-- | Standard usage:
-- |
-- | ```purescript
-- | GenServer.startLink $ GenServer.defaultSpec init
-- |   where
-- |   init :: InitFn Unit Unit Unit {}
-- |   init = pure $ InitOk {}
-- | ```
startLink :: forall cont stop msg state. (ServerSpec cont stop msg state) -> Effect (StartLinkResult (ServerPid cont stop msg state))
startLink { name: maybeName, init: initFn, handleInfo, handleContinue, terminate: terminate', trapExits } = startLinkFFI maybeName (nativeModuleName pintoGenServer) initEffect
  where
  context =
    Context
      { handleInfo
      , handleContinue
      , terminate: terminate'
      , trapExits
      }

  initEffect :: Effect (InitResult cont (OuterState cont stop msg state))
  initEffect = do
    _ <- case trapExits of
      Nothing -> pure unit
      Just _ -> void $ setProcessFlagTrapExit true
    innerResult <- (runReaderT $ case initFn of ResultT inner -> inner) context
    pure $ mapInitResult (mkOuterState context) innerResult

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
  , terminate :: Maybe (TerminateFn cont stop msg state)
  , trapExits :: Maybe (ExitMessage -> msg)
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

foreign import replyToFFI :: forall reply. From reply -> reply -> Effect Unit

replyTo :: forall cont stop msg state reply. From reply -> reply -> ResultT cont stop msg state Unit
replyTo from reply = Lift.liftEffect $ replyToFFI from reply

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

foreign import startLinkFFI ::
  forall cont stop msg state.
  Maybe (RegistryName (ServerType cont stop msg state)) ->
  NativeModuleName ->
  Effect (InitResult cont (OuterState cont stop msg state)) ->
  Effect (StartLinkResult (ServerPid cont stop msg state))

instance resultT_HasSelf :: HasSelf (ResultT cont stop msg state) msg where
  self = do
    serverPid :: (ServerPid cont stop msg state) <- Lift.liftEffect selfFFI
    pure $ getProcess serverPid

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
    exportInitResult <$> impl

handle_call :: forall reply cont stop msg state. EffectFn3 (CallFn reply cont stop msg state) (From reply) (OuterState cont stop msg state) (NativeCallResult reply cont stop (OuterState cont stop msg state))
handle_call =
  mkEffectFn3 \f from _state@{ innerState, context } -> do
    result <- (runReaderT $ case f from innerState of ResultT inner -> inner) context
    pure $ exportCallResult (mkOuterState context <$> result)

handle_cast :: forall cont stop msg state. EffectFn2 (CastFn cont stop msg state) (OuterState cont stop msg state) (NativeReturnResult cont stop (OuterState cont stop msg state))
handle_cast =
  mkEffectFn2 \f _state@{ innerState, context } -> do
    result <- (runReaderT $ case f innerState of ResultT inner -> inner) context
    pure $ exportReturnResult (mkOuterState context <$> result)

handle_info :: forall cont stop msg state. EffectFn2 Foreign (OuterState cont stop msg state) (NativeReturnResult cont stop (OuterState cont stop msg state))
handle_info =
  mkEffectFn2 \nativeMsg state@{ innerState, context: context@(Context { handleInfo: maybeHandleInfo, trapExits }) } ->
    exportReturnResult
      <$> case maybeHandleInfo of
          Just f -> do
            let
              exitMessage = parseTrappedExitFFI nativeMsg Exit
              processMsg =
                mkFn2
                  ( \f msg -> do
                      ReturnResult mAction state <- (runReaderT $ case f msg innerState of ResultT inner -> inner) context
                      -- pure $ (mkOuterState context <$> result)
                      pure $ ReturnResult mAction (mkOuterState context state)
                  )
              msg :: msg
              msg = unsafeFromForeign nativeMsg
            maybe (runFn2 processMsg f msg) (\_ -> maybe (pure $ ReturnResult Nothing state) (\e -> runFn2 processMsg f e) (trapExits <*> exitMessage)) exitMessage
          Nothing ->
            pure $ ReturnResult Nothing state

terminate :: forall cont stop msg state. EffectFn2 Foreign (OuterState cont stop msg state) Atom
terminate =
  mkEffectFn2 \reason _state@{ innerState, context: context@(Context { terminate: maybeTerminate }) } -> do
    case maybeTerminate of
      Just f -> (runReaderT $ case f (parseShutdownReasonFFI reason) innerState of ResultT inner -> inner) context
      Nothing -> pure unit
    pure $ atom "ok"

handle_continue :: forall cont stop msg state. EffectFn2 cont (OuterState cont stop msg state) (NativeReturnResult cont stop (OuterState cont stop msg state))
handle_continue =
  mkEffectFn2 \msg state@{ innerState, context: context@(Context { handleContinue: maybeHandleContinue }) } ->
    exportReturnResult
      <$> case maybeHandleContinue of
          Just f -> do
            result <- (runReaderT $ case f msg innerState of ResultT inner -> inner) context
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
    CallResult Nothing Nothing newState -> unsafeCoerce $ tuple2 (atom "noreply") newState
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
