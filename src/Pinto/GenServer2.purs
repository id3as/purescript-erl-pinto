-- | Module representing the gen_server in OTP
-- | See also 'gen_server' in the OTP docs (https://erlang.org/doc/man/gen_server.html)


module Pinto.GenServer2
  ( Action(..)
  , CallFn
  , CallResult(..)
  , CastFn
  , Context
  , ContinueFn
  , From
  , InfoFn
  , InitFn
  , InitResult(..)
  , NativeCallResult
  , NativeInitResult
  , NativeReturnResult
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
  , handle_info
  , init
  , noReply
  , noReplyWithAction
  , reply
  , replyTo
  , replyWithAction
  , return
  , returnWithAction
  , startLink3
  )
  where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3)
import Erl.Atom (atom)
import Erl.Data.List (List, head)
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.ModuleName (NativeModuleName, nativeModuleName)
import Erl.Process (class HasProcess, Process, ProcessM)
import Erl.Process.Raw (class HasPid)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Pinto.ModuleNames (pintoGenServer)
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans, initialise, parseForeign, run)
import Pinto.Types (RegistryInstance, RegistryName, RegistryReference, ShutdownReason, StartLinkResult, registryInstance)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)



data TestState = TestState Int
data TestCont = TestCont
data TestStop = TestStop
data TestMsg = TestMsg

data TestMonitorMsg = TestMonitorMsg




foreign import data FromForeign :: Type
newtype From :: Type -> Type
newtype From reply
  = From FromForeign



newtype ServerPid cont stop appMsg state m
  = ServerPid (Process appMsg)

derive newtype instance Eq (ServerPid cont stop appMsg state m)
derive newtype instance HasPid (ServerPid cont stop appMsg state m)
derive newtype instance HasProcess appMsg (ServerPid const stop appMsg state m)

instance Show (ServerPid cont stop appMsg state m) where
  show (ServerPid pid) = "(ServerPid " <> show pid <> ")"


-- | The typed reference of a GenServer, containing all the information required to get hold of
-- | an instance
type ServerRef cont stop appMsg state m
  = RegistryReference (ServerPid cont stop appMsg state m) (ServerType cont stop state m)

-- | The typed instance of a GenServer, containing all the information required to call into
-- | a GenServer
type ServerInstance cont stop appMsg state m
  = RegistryInstance (ServerPid cont stop appMsg state m) (ServerType cont stop appMsg state)

-- | Given a RegistryName with a valid (ServerType), get hold of a typed Process `msg` to which messages
-- | can be sent (arriving in the handleInfo callback)
foreign import whereIs :: forall cont stop appMsg state m. RegistryName (ServerType cont stop appMsg state) -> Effect (Maybe (ServerPid cont stop appMsg state m))


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



type InitFn :: forall k. Type -> Type -> (Type -> k) -> k
type InitFn cont state m                =                            m (InitResult cont state)
type InfoFn cont stop parsedMsg state m = parsedMsg ->      state -> m (ReturnResult cont stop state)
type ContinueFn cont stop state m       = cont ->           state -> m (ReturnResult cont stop state)
type CastFn cont stop state m           =                   state -> m (ReturnResult cont stop state)
type CallFn reply cont stop state m     = From reply ->     state -> m (CallResult reply cont stop state)
type TerminateFn state m                = ShutdownReason -> state -> m  Unit



--type GSMonad = MonitorT TestMonitorMsg (ProcessM TestMsg)
type GSMonad = ProcessM TestMsg





-- | The various return values from an init callback
-- | These roughly map onto the tuples in the OTP documentation
data InitResult cont state
  = InitOk state
  | InitOkTimeout state Int
  | InitOkContinue state cont
  | InitOkHibernate state
  | InitStop Foreign
  | InitIgnore

instance Functor (InitResult cont) where
  map f (InitOk state) = InitOk $ f state
  map f (InitOkTimeout state timeout) = InitOkTimeout (f state) timeout
  map f (InitOkContinue state cont) = InitOkContinue (f state) cont
  map f (InitOkHibernate state) = InitOkHibernate $ f state
  map _ (InitStop term) = InitStop term
  map _ InitIgnore = InitIgnore



fi :: InitFn TestCont TestState GSMonad
fi = fooInit

fooInit :: GSMonad (InitResult TestCont TestState)
fooInit = do
  --_ <- spawnMonitor exitsImmediately $ const TestMonitorMsg
  pure $ InitOk $ TestState 0




--fooHI :: _ -> _ -> GSMonad _ --(ReturnResult TestCont TestStop TestState)
fooHI msg state = do
  case msg of
    --Left TestMonitorMsg ->  pure $ return state
    TestMsg ->  pure $ return state


--x :: Record (AllConfig TestCont TestStop (Either TestMonitorMsg TestMsg) TestState (MonitorT TestMonitorMsg (ProcessM TestMsg)))
-- x =
--   startLink
--      { init
--      , handleInfo: Just fooHI
--      }
--   where
--     init :: InitFn TestCont TestState GSMonad
--     init = do
--       --_ <- spawnMonitor exitsImmediately $ const TestMonitorMsg
--       pure $ InitOk $ TestState 0


-- y =
--   startLink2 (Proxy :: Proxy GSMonad)
--      { init
--      , handleInfo: Just fooHI
--      }
--   where
--     init = do
--       --_ <- spawnMonitor exitsImmediately $ const TestMonitorMsg
--       pure $ InitOk $ TestState 0



z =
  startLink' { init
             , handleContinue
             }
  where
    init :: InitFn _ _ GSMonad
    init = do
      --_ <- spawnMonitor exitsImmediately $ const TestMonitorMsg
      pure $ InitOk $ TestState 0

    handleContinue _ state = pure $ return state

--newtype ServerType :: Type -> Type -> Type -> Type -> Type
newtype ServerType cont stop state m
  = ServerType Void



type OptionalConfig cont stop parsedMsg state m =
  ( name           :: Maybe (RegistryName (ServerType cont stop state m))
  , handleInfo     :: Maybe (InfoFn cont stop parsedMsg state m)
  , handleContinue :: Maybe (ContinueFn cont stop state m)
  , terminate      :: Maybe (TerminateFn state m)
  )

--type X cont stop msg parsedMsg state m = Record (MyOptional cont stop msg parsedMsg state m)

type AllConfig cont stop parsedMsg state m   =
  ( init ::  InitFn cont state m
  | OptionalConfig cont stop parsedMsg state m
  )

type GSConfig cont stop parsedMsg state m =
  { | AllConfig cont stop parsedMsg state m }


data TransState
data TransMsg
data TransMonad
data TransRes

newtype Context cont stop parsedMsg state m
  = Context
    { handleInfo     :: Maybe (InfoFn cont stop parsedMsg state m)
    , handleContinue :: Maybe (ContinueFn cont stop state m)
    , terminate      :: Maybe (TerminateFn state m)
    , mState         :: TransState
    , mParse         :: Foreign -> TransMsg
    , mRun           :: TransMonad -> TransState -> Effect (Tuple TransRes TransState)
    }



type OuterState cont stop parsedMsg state m
  = { innerState :: state
    , context :: Context cont stop parsedMsg state m
    }

foreign import startLinkFFI ::
  forall cont stop appMsg parsedMsg state m.
  Maybe (RegistryName (ServerType cont stop state m)) ->
  NativeModuleName ->
  Effect (InitResult cont (OuterState cont stop parsedMsg state m)) ->
  Effect (StartLinkResult (ServerPid cont stop appMsg state m))


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
startLink3
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => GSConfig cont stop parsedMsg state m
  -> Effect (StartLinkResult (ServerPid cont stop appMsg state m))
startLink3 { name: maybeName, init: initFn, handleInfo, handleContinue, terminate: terminate' }
  = startLinkFFI maybeName (nativeModuleName pintoGenServer) initEffect
  where
  initEffect :: Effect (InitResult cont (OuterState cont stop parsedMsg state m))
  initEffect = do
    initialMState <- initialise (Proxy :: Proxy m)
    Tuple innerResult newMState  <- run initFn initialMState
    pure $ { context: initialContext newMState, innerState: _ } <$> innerResult

  initialContext :: mState -> Context cont stop parsedMsg state m
  initialContext mState = Context { handleInfo : handleInfo
                                  , handleContinue : handleContinue
                                  , terminate: terminate'
                                  , mState : unsafeCoerce mState
                                  , mParse : unsafeCoerce (parseForeign :: (Foreign -> m parsedMsg))
                                  , mRun : unsafeCoerce (run :: forall a. m a -> mState -> Effect (Tuple a mState))
                                  }


foreign import callFFI
  :: forall reply cont stop appMsg state m
   . ServerInstance cont stop appMsg state m
  -> CallFn reply cont stop state m
  -> Effect reply

call
  :: forall reply cont stop appMsg state m
   . ServerRef cont stop appMsg state m
  -> CallFn reply cont stop state m
  -> Effect reply
call r callFn = callFFI (registryInstance r) callFn

foreign import castFFI
  :: forall cont stop appMsg state m
   . ServerInstance cont stop appMsg state m
  -> CastFn cont stop state m
  -> Effect Unit

cast
  :: forall reply cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => ServerRef cont stop appMsg state m
  -> CastFn cont stop state m
  -> Effect Unit
cast r castFn = do
  let
    x :: ?t
    x = registryInstance r

  castFFI (unsafeCoerce 1) castFn


foreign import replyToFFI :: forall reply. From reply -> reply -> Effect Unit
replyTo
  :: forall reply m
   . MonadEffect m
  => From reply -> reply -> m Unit
replyTo from reply = liftEffect $ replyToFFI from reply


defaultSpec
  :: forall cont stop parsedMsg appMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => InitFn cont state m
  -> GSConfig cont stop parsedMsg state m
defaultSpec initFn =
  { name: Nothing
  , init: initFn
  , handleInfo: Nothing
  , handleContinue: Nothing
  , terminate: Nothing
  }


defaultOptions :: forall cont stop parsedMsg state m. { | OptionalConfig cont stop parsedMsg state m }
defaultOptions
  = { name : Nothing
    , handleInfo : Nothing
    , handleContinue : Nothing
    , terminate: Nothing
    }

exitsImmediately :: ProcessM Void Unit
exitsImmediately = pure unit


data OptionToMaybe
  = OptionToMaybe

-- instance ConvertOption OptionToMaybe "handleInfo" (Maybe (InfoFn cont stop parsedMsg state m)) (Maybe (InfoFn cont stop parsedMsg state m)) where
--   convertOption _ _ val = val
-- else instance ConvertOption OptionToMaybe "handleInfo" (InfoFn cont stop parsedMsg state m) (Maybe (InfoFn cont stop parsedMsg state m)) where
--   convertOption _ _ val = Just val
instance ConvertOption OptionToMaybe "handleInfo" (Maybe a) (Maybe a) where
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

-- a = startLink' { init
--                  --                                         , handleContinue : handleContinue
--   --             , terminate : terminate
--                , handleInfo : handleInfo
--                }
--   where
-- --    init :: InitFn TestCont TestState GSMonad
--     init = do
--       pure $ InitOk $ TestState 0


--     --handleInfo ::  TestMsg -> TestState -> ProcessM TestMsg (ReturnResult TestCont TestStop TestState)
--     --handleInfo ::  InfoFn TestCont TestStop TestMsg TestState GSMonad
--     handleInfo msg state = do
--       case msg of
--         --Left TestMonitorMsg ->  pure $ return state
--         TestMsg ->  pure $ return state


--     -- handleContinue :: ContinueFn TestCont TestStop TestState GSMonad
--     handleContinue _ state = pure $ return state

-- --    terminate :: TerminateFn TestState GSMonad
--     terminate _ _ = do
--       pure unit



-- b = startLink { init
--               , handleInfo : Nothing
--               , handleContinue : Nothing
--               , terminate: Nothing --Just terminate
--               }

--   where
--     init :: InitFn TestCont TestState GSMonad
--     init = do
--       pure $ InitOk $ TestState 0


--     --handleInfo ::  TestMsg -> TestState -> ProcessM TestMsg (ReturnResult TestCont TestStop TestState)
--     --handleInfo ::  InfoFn TestCont TestStop TestMsg TestState GSMonad
--     handleInfo msg state = do
--       case msg of
--         --Left TestMonitorMsg ->  pure $ return state
--         TestMsg ->  pure $ return state


--     -- handleContinue :: ContinueFn TestCont TestStop TestState GSMonad
--     handleContinue _ state = pure $ return state

--     terminate :: TerminateFn TestState GSMonad
--     terminate _ _ = do
--       pure unit


startLink'
  :: forall providedConfig cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => ConvertOptionsWithDefaults OptionToMaybe { | OptionalConfig cont stop parsedMsg state m} { | providedConfig } { | AllConfig cont stop parsedMsg state m}
  => { | providedConfig } -> String
startLink' providedConfig =
  let
    config = convertOptionsWithDefaults OptionToMaybe defaultOptions providedConfig
    _ = spy "config" config
  in
    startLink config
  
startLink
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => { | AllConfig cont stop parsedMsg state m } -> String
startLink config = "hello"

startLink2
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Proxy m -> { | AllConfig cont stop parsedMsg state m } -> String
startLink2 _ = startLink





--------------------------------------------------------------------------------
-- Underlying gen_server callback that defer to the provided monadic handlers
--------------------------------------------------------------------------------
foreign import data NativeInitResult :: Type -> Type
foreign import data NativeCallResult :: Type -> Type -> Type -> Type -> Type
foreign import data NativeReturnResult :: Type -> Type -> Type -> Type

init ::
  forall cont state.
  EffectFn1 (List (Effect (InitResult cont state))) (NativeInitResult state)
init =
  mkEffectFn1 \args -> do
    let
      impl = unsafePartial $ fromJust $ head args
    exportInitResult <$> impl

-- handle_call
--   :: forall reply cont stop msg state m.
--      EffectFn3 (CallFn reply cont stop msg state m) (From reply) (OuterState cont stop msg state) (NativeCallResult reply cont stop (OuterState cont stop msg state))
-- handle_call =
--   mkEffectFn3 \f from _state@{ innerState, context } -> do
--     result <- case f from innerState of ResultT inner -> inner
--     pure $ exportCallResult (mkOuterState context <$> result)



handle_info
  :: forall m cont stop parsedMsg state mState.
     EffectFn2 Foreign (OuterState cont stop parsedMsg state m) (NativeReturnResult cont stop (OuterState cont stop parsedMsg state m))
handle_info =
  mkEffectFn2 \nativeMsg state@{ innerState, context: Context ctx@{ handleInfo: maybeHandleInfo, mState, mParse, mRun } } ->
    exportReturnResult
      <$> case maybeHandleInfo of
          Just f -> do
            let
              -- We had to forget the types of the typeclass members when we stored them - remember them again
              mParse' :: Foreign -> m parsedMsg
              mParse' = unsafeCoerce mParse
              mRun' :: forall a mState. m a -> mState -> Effect (Tuple a mState)
              mRun'  = unsafeCoerce mRun
              mState' :: mState
              mState' = unsafeCoerce mState
            Tuple parsedMsg newMState <- mRun' (mParse' nativeMsg) mState'
            Tuple (ReturnResult mAction state) newMState' <- mRun' (f parsedMsg innerState) newMState
            pure $ ReturnResult mAction (mkOuterState2 (Context ctx{mState = unsafeCoerce newMState'}) state)
          Nothing ->
            -- If you don't set up a handler then we choose to ignore all messages rather than crash...
            pure $ ReturnResult Nothing state


mkOuterState2 = { context: _, innerState: _ }
--------------------------------------------------------------------------------
-- Helpers to construct the appropriate erlang tuples from the GenServer ADTs
--------------------------------------------------------------------------------
exportInitResult :: forall cont state. InitResult cont state -> NativeInitResult state
exportInitResult = case _ of
  InitStop err -> unsafeCoerce $ tuple2 (atom "stop") err
  InitIgnore -> unsafeCoerce $ atom "ignore"
  InitOk state -> unsafeCoerce $ tuple2 (atom "ok") state
  InitOkTimeout state timeout -> unsafeCoerce $ tuple3 (atom "timeout") state timeout
  InitOkContinue state cont -> unsafeCoerce $ tuple3 (atom "ok") state $ tuple2 (atom "continue") cont
  InitOkHibernate state -> unsafeCoerce $ tuple3 (atom "ok") state (atom "hibernate")

exportReturnResult :: forall cont stop outerState. ReturnResult cont stop outerState -> NativeReturnResult cont stop outerState
exportReturnResult = case _ of
  ReturnResult Nothing newState -> unsafeCoerce $ tuple2 (atom "noreply") newState
  ReturnResult (Just (Timeout timeout)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState timeout
  ReturnResult (Just Hibernate) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ atom "hibernate"
  ReturnResult (Just (Continue cont)) newState -> unsafeCoerce $ tuple3 (atom "noreply") newState $ tuple2 (atom "continue") cont
  ReturnResult (Just StopNormal) newState -> unsafeCoerce $ tuple3 (atom "stop") (atom "normal") newState
  ReturnResult (Just (StopOther reason)) newState -> unsafeCoerce $ tuple3 (atom "stop") reason newState
