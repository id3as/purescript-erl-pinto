-- | Module representing the gen_server in OTP
-- | See also 'gen_server' in the OTP docs (https://erlang.org/doc/man/gen_server.html)

module Pinto.GenServer.ContStop
  ( Action(..)
  , AllConfig
  , CallFn
  , CallResult(..)
  , CastFn
  , ContinueFn
  , From
  , GSConfig
  , InfoFn
  , InitFn
  , InitResult(..)
  , NativeCallResult
  , NativeInitResult
  , NativeReturnResult
  , OTPState
  , OptionToMaybe
  , OptionalConfig
  , ReturnResult(..)
  , ServerInstance
  , ServerPid
  , ServerRef
  , ServerType
  , TerminateFn
  , TestState
  , call
  , cast
  , defaultSpec
  , exportInitResult
  , exportReturnResult
  , handle_call
  , handle_cast
  , handle_continue
  , handle_info
  , init
  , noReply
  , noReplyWithAction
  , reply
  , replyTo
  , replyWithAction
  , return
  , returnWithAction
  , startLink
  , startLink'
  , stop
  , terminate
  , whereIs
  ) where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List, head)
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Erl.ModuleName (NativeModuleName, nativeModuleName)
import Erl.Process (class HasProcess, ProcessM)
import Erl.Process.Raw (class HasPid)
import Erl.Process.Raw as Raw
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Pinto.ModuleNames (pintoGenServerCS)
import Pinto.ProcessT.Internal.Types (class MonadProcessHandled, class MonadProcessTrans, initialise, parseForeign, run)
import Pinto.ProcessT.MonitorT (MonitorT)
import Pinto.Types (RegistryInstance, RegistryName, RegistryReference, ShutdownReason, StartLinkResult, parseShutdownReasonFFI, registryInstance)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data TestState = TestState Int
data TestCont = TestCont
data TestStop = TestStop
data TestMsg = TestMsg

data TestMonitorMsg = TestMonitorMsg

foreign import data FromForeign :: Type

newtype From :: Type -> Type
newtype From reply = From FromForeign

newtype ServerPid :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Type
newtype ServerPid cont stop state m = ServerPid Raw.Pid

derive newtype instance Eq (ServerPid cont stop state m)

instance
  ( MonadProcessTrans m innerState appMsg parsedMsg
  ) =>
  HasProcess appMsg (ServerPid const stop state m) where
  getProcess (ServerPid rawPid) = unsafeCoerce rawPid

instance
  ( MonadProcessTrans m innerState appMsg parsedMsg
  , Monad m
  ) =>
  HasPid (ServerPid const stop state m) where
  getPid = unsafeCoerce

instance Show (ServerPid cont stop state m) where
  show (ServerPid pid) = "(ServerPid " <> show pid <> ")"

-- | The typed reference of a GenServer, containing all the information required to get hold of
-- | an instance
type ServerRef :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Type
type ServerRef cont stop state m = RegistryReference (ServerPid cont stop state m) (ServerType cont stop state m)

-- | The typed instance of a GenServer, containing all the information required to call into
-- | a GenServer
type ServerInstance :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Type
type ServerInstance cont stop state m = RegistryInstance (ServerPid cont stop state m) (ServerType cont stop state m)

-- | Given a RegistryName with a valid (ServerType), get hold of a typed Process `msg` to which messages
-- | can be sent (arriving in the handleInfo callback)
foreign import whereIs :: forall cont stop state m. RegistryName (ServerType cont stop state m) -> Effect (Maybe (ServerPid cont stop state m))

-- | An action to be returned to OTP
-- | See {shutdown, reason}, {timeout...} etc in the gen_server documentation
-- | This should be constructed and returned with the xxWithAction methods inside GenServer callbacks
data Action cont stop
  = Hibernate
  | Continue cont
  | StopNormal
  | StopOther stop

-- | The result of a GenServer.call (handle_call) action
data CallResult reply cont stop state = CallResult (Maybe reply) (Maybe (Action cont stop)) state

instance mapCallResult :: Functor (CallResult reply cont stop) where
  map f (CallResult mReply mAction state) = CallResult mReply mAction (f state)

-- | The result of a GenServer.handle_info or GenServer.handle_cast callback
data ReturnResult cont stop state = ReturnResult (Maybe (Action cont stop)) state

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

type InitFn :: forall k. Type -> Type -> (Type -> k) -> k
type InitFn cont state m = m (InitResult cont state)

type InfoFn cont stop parsedMsg state m = parsedMsg -> state -> m (ReturnResult cont stop state)
type ContinueFn cont stop state m = cont -> state -> m (ReturnResult cont stop state)
type CastFn cont stop state m = state -> m (ReturnResult cont stop state)
type CallFn reply cont stop state m = From reply -> state -> m (CallResult reply cont stop state)
type TerminateFn state m = ShutdownReason -> state -> m Unit

type GSMonad = MonitorT TestMonitorMsg (ProcessM TestMsg)
--type GSMonad = ProcessM TestMsg

-- | The various return values from an init callback
-- | These roughly map onto the tuples in the OTP documentation
data InitResult cont state
  = InitOk state
  | InitOkContinue state cont
  | InitOkHibernate state
  | InitStop Foreign
  | InitIgnore

instance Functor (InitResult cont) where
  map f (InitOk state) = InitOk $ f state
  map f (InitOkContinue state cont) = InitOkContinue (f state) cont
  map f (InitOkHibernate state) = InitOkHibernate $ f state
  map _ (InitStop term) = InitStop term
  map _ InitIgnore = InitIgnore

--newtype ServerType :: Type -> Type -> Type -> Type -> Type
newtype ServerType :: forall k1 k2 k3 k4. k1 -> k2 -> k3 -> k4 -> Type
newtype ServerType cont stop state m = ServerType Void

type OptionalConfig cont stop parsedMsg state m =
  ( serverName :: Maybe (RegistryName (ServerType cont stop state m))
  , handleInfo :: Maybe (InfoFn cont stop parsedMsg state m)
  , handleContinue :: Maybe (ContinueFn cont stop state m)
  , terminate :: Maybe (TerminateFn state m)
  )

type AllConfig cont stop parsedMsg state m =
  ( init :: InitFn cont state m
  | OptionalConfig cont stop parsedMsg state m
  )

type GSConfig cont stop parsedMsg state m =
  { | AllConfig cont stop parsedMsg state m }

data TransState
data TransMsg
data TransRes

type Context cont stop parsedMsg state m =
  { handleInfo :: Maybe (InfoFn cont stop parsedMsg state m)
  , handleContinue :: Maybe (ContinueFn cont stop state m)
  , terminate :: Maybe (TerminateFn state m)
  , mParse :: forall a. Foreign -> a
  , mRun :: forall a m. m a -> TransState -> Effect (Tuple a TransState)
  }

newtype OTPState cont stop parsedMsg state m = OTPState
  { innerState :: state
  , mState :: TransState
  , context :: Context cont stop parsedMsg state m
  }

foreign import startLinkFFI
  :: forall cont stop parsedMsg state m
   . Maybe (RegistryName (ServerType cont stop state m))
  -> NativeModuleName
  -> Effect (InitResult cont (OTPState cont stop parsedMsg state m))
  -> Effect (StartLinkResult (ServerPid cont stop state m))

startLink
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessHandled m parsedMsg
  => MonadProcessTrans m mState appMsg parsedMsg
  => GSConfig cont stop parsedMsg state m
  -> Effect (StartLinkResult (ServerPid cont stop state m))
startLink { serverName: maybeName, init: initFn, handleInfo, handleContinue, terminate: terminate' } = startLinkFFI maybeName (nativeModuleName pintoGenServerCS) initEffect
  where
  initEffect :: Effect (InitResult cont (OTPState cont stop parsedMsg state m))
  initEffect = do
    initialMState <- initialise (Proxy :: Proxy m)
    Tuple innerResult newMState <- run initFn initialMState

    pure $ OTPState
      <$>
        { context
        , mState: unsafeCoerce newMState -- TODO - add in the bug again (initialMState) and have a failing test
        , innerState: _
        }
      <$> innerResult

  context :: Context cont stop parsedMsg state m
  context =
    { handleInfo: handleInfo
    , handleContinue: handleContinue
    , terminate: terminate'
    , mParse: unsafeCoerce (parseForeign :: (Foreign -> m (Maybe parsedMsg)))
    , mRun: unsafeCoerce (run :: forall a. m a -> mState -> Effect (Tuple a mState))
    }

startLink'
  :: forall providedConfig cont stop appMsg parsedMsg state m mState
   . MonadProcessHandled m parsedMsg
  => MonadProcessTrans m mState appMsg parsedMsg
  => ConvertOptionsWithDefaults OptionToMaybe { | OptionalConfig cont stop parsedMsg state m } { | providedConfig } { | AllConfig cont stop parsedMsg state m }
  => { | providedConfig }
  -> Effect (StartLinkResult (ServerPid cont stop state m))
startLink' providedConfig =
  startLink $ convertOptionsWithDefaults OptionToMaybe defaultOptions providedConfig

foreign import callFFI
  :: forall reply cont stop state m
   . ServerInstance cont stop state m
  -> CallFn reply cont stop state m
  -> Effect reply

call
  :: forall reply cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m -- TODO - why is the monad constraint needed?
  => ServerRef cont stop state m
  -> CallFn reply cont stop state m
  -> Effect reply
call r callFn = callFFI (registryInstance r) callFn

foreign import castFFI
  :: forall cont stop state m
   . ServerInstance cont stop state m
  -> CastFn cont stop state m
  -> Effect Unit

cast
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m
  => ServerRef cont stop state m
  -> CastFn cont stop state m
  -> Effect Unit
cast r castFn = castFFI (registryInstance r) castFn

foreign import stopFFI
  :: forall cont stop state m
   . ServerInstance cont stop state m
  -> Effect Unit

stop
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m
  => ServerRef cont stop state m
  -> Effect Unit
stop r = stopFFI $ registryInstance r

foreign import replyToFFI :: forall reply. From reply -> reply -> Effect Unit

replyTo
  :: forall reply m
   . MonadEffect m
  => From reply
  -> reply
  -> m Unit
replyTo from reply = liftEffect $ replyToFFI from reply

defaultSpec
  :: forall cont stop parsedMsg appMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => InitFn cont state m
  -> GSConfig cont stop parsedMsg state m
defaultSpec initFn =
  { serverName: Nothing
  , init: initFn
  , handleInfo: Nothing
  , handleContinue: Nothing
  , terminate: Nothing
  }

defaultOptions :: forall cont stop parsedMsg state m. { | OptionalConfig cont stop parsedMsg state m }
defaultOptions =
  { serverName: Nothing
  , handleInfo: Nothing
  , handleContinue: Nothing
  , terminate: Nothing
  }

data OptionToMaybe = OptionToMaybe

instance ConvertOption OptionToMaybe "serverName" (Maybe a) (Maybe a) where
  convertOption _ _ val = val
else instance ConvertOption OptionToMaybe "serverName" a (Maybe a) where
  convertOption _ _ val = Just val
else instance ConvertOption OptionToMaybe "handleInfo" (Maybe a) (Maybe a) where
  convertOption _ _ val = val
else instance ConvertOption OptionToMaybe "handleInfo" a (Maybe a) where
  convertOption _ _ val = Just val
else instance ConvertOption OptionToMaybe "handleContinue" (Maybe a) (Maybe a) where
  convertOption _ _ val = val
else instance ConvertOption OptionToMaybe "handleContinue" a (Maybe a) where
  convertOption _ _ val = Just val
else instance ConvertOption OptionToMaybe "terminate" (Maybe a) (Maybe a) where
  convertOption _ _ val = val
else instance ConvertOption OptionToMaybe "terminate" a (Maybe a) where
  convertOption _ _ val = Just val
else instance ConvertOption OptionToMaybe sym a a where
  convertOption _ _ val = val

--------------------------------------------------------------------------------
-- Underlying gen_server callback that defer to the provided monadic handlers
--------------------------------------------------------------------------------
foreign import data NativeInitResult :: Type -> Type
foreign import data NativeCallResult :: Type -> Type -> Type -> Type -> Type
foreign import data NativeReturnResult :: Type -> Type -> Type -> Type

init
  :: forall cont state
   . EffectFn1 (List (Effect (InitResult cont state))) (NativeInitResult state)
init =
  mkEffectFn1 \args -> do
    let
      impl = unsafePartial $ fromJust $ head args
    exportInitResult <$> impl

handle_call
  :: forall reply cont stop parsedMsg state m
   . EffectFn3 (CallFn reply cont stop state m) (From reply) (OTPState cont stop parsedMsg state m) (NativeCallResult reply cont stop (OTPState cont stop parsedMsg state m))
handle_call =
  mkEffectFn3 \f from otpState@(OTPState { innerState, mState, context: { mRun } }) -> do
    Tuple result newMState <- mRun (f from innerState) mState
    pure $ exportCallResult (updateOtpState otpState newMState <$> result)

handle_cast
  :: forall cont stop parsedMsg state m
   . EffectFn2 (CastFn cont stop state m) (OTPState cont stop parsedMsg state m) (NativeReturnResult cont stop (OTPState cont stop parsedMsg state m))
handle_cast =
  mkEffectFn2 \f otpState@(OTPState { innerState, mState, context: { mRun } }) -> do
    Tuple result newMState <- mRun (f innerState) mState
    pure $ exportReturnResult (updateOtpState otpState newMState <$> result)

handle_continue
  :: forall m cont stop parsedMsg state
   . EffectFn2 cont (OTPState cont stop parsedMsg state m) (NativeReturnResult cont stop (OTPState cont stop parsedMsg state m))
handle_continue =
  mkEffectFn2 \contMsg otpState@(OTPState { innerState, mState, context: { handleContinue: maybeHandleContinue, mRun } }) ->
    exportReturnResult <$>
      case maybeHandleContinue of
        Just f -> do
          Tuple result newMState <- mRun (f contMsg innerState) mState
          pure $ updateOtpState otpState newMState <$> result
        Nothing ->
          pure $ ReturnResult Nothing otpState

handle_info
  :: forall m cont stop parsedMsg state
   . EffectFn2 Foreign (OTPState cont stop parsedMsg state m) (NativeReturnResult cont stop (OTPState cont stop parsedMsg state m))
handle_info =
  mkEffectFn2 \nativeMsg otpState@(OTPState { innerState, mState, context: { handleInfo: maybeHandleInfo, mParse, mRun } }) ->
    exportReturnResult <$>
      case maybeHandleInfo of
        Just f -> do
          Tuple mParsedMsg newMState <- mRun (mParse nativeMsg) mState
          case mParsedMsg of
            Nothing ->
              pure $ ReturnResult Nothing $ updateMonadState otpState newMState
            Just parsedMsg -> do
              Tuple result newMState' <- mRun (f parsedMsg innerState) newMState
              pure $ updateOtpState otpState newMState' <$> result
        Nothing ->
          pure $ ReturnResult Nothing otpState

terminate
  :: forall cont stop parsedMsg state m
   . EffectFn2 Foreign (OTPState cont stop parsedMsg state m) Atom
terminate =
  mkEffectFn2 \reason (OTPState { innerState, mState, context: { mRun, terminate } }) -> do
    case terminate of
      Just f -> void $ mRun (f (parseShutdownReasonFFI reason) innerState) mState
      Nothing -> pure unit
    pure $ atom "ok"

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------
updateOtpState :: forall cont stop parsedMsg state m. OTPState cont stop parsedMsg state m -> TransState -> state -> OTPState cont stop parsedMsg state m
updateOtpState (OTPState otpState) mState innerState = OTPState otpState { mState = mState, innerState = innerState }

updateMonadState :: forall cont stop parsedMsg state m. OTPState cont stop parsedMsg state m -> TransState -> OTPState cont stop parsedMsg state m
updateMonadState (OTPState otpState) mState = OTPState otpState { mState = mState }

--------------------------------------------------------------------------------
-- Helpers to construct the appropriate erlang tuples from the GenServer ADTs
--------------------------------------------------------------------------------
exportInitResult :: forall cont state. InitResult cont state -> NativeInitResult state
exportInitResult = case _ of
  InitStop err -> unsafeCoerce $ tuple2 (atom "stop") err
  InitIgnore -> unsafeCoerce $ atom "ignore"
  InitOk state -> unsafeCoerce $ tuple2 (atom "ok") state
  InitOkContinue state cont -> unsafeCoerce $ tuple3 (atom "ok") state $ tuple2 (atom "continue") cont
  InitOkHibernate state -> unsafeCoerce $ tuple3 (atom "ok") state (atom "hibernate")

exportReturnResult :: forall cont stop outerState. ReturnResult cont stop outerState -> NativeReturnResult cont stop outerState
exportReturnResult = case _ of
  ReturnResult Nothing newState -> unsafeCoerce $ tuple2 (atom "noreply") newState
  ReturnResult (Just Hibernate) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ atom "hibernate"
  ReturnResult (Just (Continue cont)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ tuple2 (atom "continue") cont
  ReturnResult (Just StopNormal) newState -> unsafeCoerce $ tuple3 (atom "stop") (atom "normal") newState
  ReturnResult (Just (StopOther reason)) newState -> unsafeCoerce $ tuple3 (atom "stop") reason newState

exportCallResult :: forall reply cont stop outerState. CallResult reply cont stop outerState -> NativeCallResult reply cont stop outerState
exportCallResult = case _ of
  CallResult (Just r) Nothing newState -> unsafeCoerce $ tuple3 (atom "reply") r newState
  CallResult (Just r) (Just Hibernate) newState -> unsafeCoerce $ tuple4 (atom "reply") r newState (atom "hibernate")
  CallResult (Just r) (Just (Continue cont)) newState -> unsafeCoerce $ tuple4 (atom "reply") r newState $ tuple2 (atom "continue") cont
  CallResult (Just r) (Just StopNormal) newState -> unsafeCoerce $ tuple4 (atom "stop") (atom "normal") r newState
  CallResult (Just r) (Just (StopOther reason)) newState -> unsafeCoerce $ tuple4 (atom "stop") reason r newState
  CallResult Nothing Nothing newState -> unsafeCoerce $ tuple2 (atom "noreply") newState
  CallResult Nothing (Just Hibernate) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState (atom "hibernate")
  CallResult Nothing (Just (Continue cont)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ tuple2 (atom "continue") cont
  CallResult Nothing (Just StopNormal) newState -> unsafeCoerce $ tuple3 (atom "stop") (atom "normal") newState
  CallResult Nothing (Just (StopOther reason)) newState -> unsafeCoerce $ tuple3 (atom "stop") reason newState
