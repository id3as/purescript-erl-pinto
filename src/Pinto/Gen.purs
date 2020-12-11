
-- | See also 'gen_server' in the OTP docs
module Pinto.Gen ( startLink
                 , buildStartLink
                 , StartLinkBuilder(..)
                 , defaultStartLink
                 , stop
                 , CallResult(..)
                 , CastResult(..)
                 , InitResult(..)
                 , doCall
                 , init
                 , doCast
                 , defaultHandleInfo
                 , whereIs
                 , monitor
                 , self
                 , GenContext
                 , StateImpl
                 , GenResultT
                 , ExitMessage(..)
                 , module Exports
                 , Call
                 , Cast
                 , Init
                 , HandleInfo
                 , handle_call
                 , handle_info
                 , terminate
                 , handle_cast
                 , CallResultImpl
                 , CastResultImpl
                 , Over
                 , With

                 , StartName(..)
                 , Handle(..)
                 , NotStartedReason(..)
                 , StartLinkResult(..)
                 )
  where

import Prelude

import Control.Monad.State (State, StateT)
import Control.Monad.State (lift) as Exports
import Control.Monad.State (runStateT, execStateT, evalStateT, lift)
import Control.Monad.State as State
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3)
import Erl.Atom (Atom, atom)
import Erl.Atom (atom)
import Erl.Data.List ((:), nil)
import Erl.Data.Tuple (tuple2, tuple3, Tuple2(..))
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process (Process(..), runProcess)
import Erl.Process.Raw (Pid)
import Foreign (Foreign, unsafeToForeign)
import Pinto.MessageRouting as MR
import Pinto.Monitor as Monitor
import Pinto.Types (RegistryName(..), ServerPid, TerminateReason)
import Unsafe.Coerce (unsafeCoerce)



-- foreign import foreignToSlr :: Foreign  -> StartLinkResult
-- foreign import foreignToScr :: Foreign  -> StartChildResult

-- foreign import slrToForeign :: StartLinkResult -> Foreign
-- foreign import scrToForeign :: StartChildResult -> Foreign



data StartName state msg
  = Named (RegistryName state msg)
  | Anonymous

data Handle state msg
  = NamedHandle (RegistryName state msg)
  | AnonymousHandle (ServerPid state msg)


data NotStartedReason state msg
  = Ignore
  | AlreadyStarted (ServerPid state msg)
  | Failed Foreign


type StartLinkResult state msg
  = Either (NotStartedReason state msg) (ServerPid state msg)


-- | The message receives by trapped exits (configured using trapExit)
data ExitMessage = Exit Pid Foreign


-- | The context of this gen server (everything except 'state' which tends to get passed into handlers
newtype GenContext state msg = GenContext { handleInfo :: msg -> state -> HandleInfo state msg
                                          , terminate :: Maybe (TerminateReason -> state -> Effect Unit)
                                          , trapExit :: Maybe (ExitMessage -> msg)
                                          , pid :: Process msg
                                          }

-- | Internal record used in the actual gen server itself
type StateImpl state msg = { innerState :: state
                           , context :: GenContext state msg }

-- | The type of any effectful callback into this gen server
type GenResultT response state msg = StateT (GenContext state msg) Effect response

-- | Type of the callback invoked during a gen_server:init
type Init state msg = GenResultT (InitResult state) state msg

--- | Type of the callback invoked during a gen_server:handle_info
type HandleInfo state msg = GenResultT (CastResult state) state msg

-- | Type of the callback invoked during a gen_server:handle_call
type Call response state msg = GenResultT (CallResult response state) state msg

-- | Type of the callback invoked during a gen_server:handle_cast
type Cast state msg = GenResultT (CastResult state) state msg

-- | Helper type for creating a function that operates in the context of a gen server and returns 'response' effectfully
type With state msg response = StateT (GenContext state msg) Effect response

-- | Helper type for creating a function that operates in the context of a gen server and operates over 'state' effectfully
type Over state msg = StateT (GenContext state msg) Effect state

foreign import enableTrapExitImpl :: Effect Unit
foreign import doCallImpl :: forall name state msg response. name -> ((StateImpl state msg) -> Pid -> Effect (CallResultImpl response state msg)) -> Effect response
foreign import doCastImpl :: forall name state msg. name -> ((StateImpl state msg) -> Effect (CastResultImpl state msg)) -> Effect Unit
foreign import stopImpl :: forall name. name -> Effect Unit
foreign import startLinkImpl :: forall name state msg. name -> Init state msg -> StartLinkBuilder state msg -> Effect Foreign
foreign import selfImpl :: forall msg.  Effect (Process msg)
foreign import whereIsImpl :: forall msg name. name -> (Process msg -> Maybe (Process msg)) -> (Maybe (Process msg)) -> Effect (Maybe (Process msg))
foreign import logWarning :: forall obj. String -> obj -> Effect Unit
foreign import mapInfoMessageImpl :: forall msg. Maybe (ExitMessage -> msg) -> (Pid -> Foreign -> ExitMessage) -> Foreign -> msg

nativeName :: forall state msg. StartName state msg -> Foreign
nativeName (Local name) = unsafeToForeign $ name
nativeName (Global name) = unsafeToForeign $ tuple2 (atom "global") name
nativeName (Via (NativeModuleName m) name) = unsafeToForeign $ tuple3 (atom "via") m name

-- | Gets the pid for this gen server
self :: forall state msg. StateT (GenContext state msg) Effect (Process msg)
self = do
  GenContext { pid } <- State.get
  pure pid

-- | Gets the pid of this gen server (if running)
-- | This is designed to be called from external agents and therefore might fail, hence the Maybe
whereIs :: forall state msg. Handle state msg -> Effect (Maybe (ServerPid state msg))
whereIs serverName = whereIsImpl (nativeName serverName) Just Nothing

-- | Short cut for monitoring a gen server via Pinto.Monitor
monitor :: forall state msg. Handle state msg -> (Monitor.MonitorMsg -> Effect Unit) -> Effect Unit -> Effect (Maybe (MR.RouterRef Monitor.MonitorRef))
monitor name cb alreadyDown = do
  maybeProcess <- whereIs name
  case maybeProcess of
    Nothing -> do
      _ <- alreadyDown
      pure Nothing
    Just p ->
      Just <$> Monitor.process p cb

-- | A typed record containing all the optional extras for configuring a genserver
type StartLinkBuilder state msg = {

    -- | A callback to be invoked when the gen server receives an arbitrary message
    handleInfo :: (msg -> state -> HandleInfo state msg)

    -- | A callback to be invoked when this gen server terminates
  , terminate :: Maybe (TerminateReason -> state -> Effect Unit)

    -- | When set to Nothing, exits will not be trapped (the default)
    -- | When there is a mapper provided for ExitMessage, trap_exits will be true
  , trapExit :: Maybe (ExitMessage -> msg)
  }

-- | Starts a typed gen-server proxy with the supplied ServerName, with the state being the result of the supplied effect
-- | This sets up the most basic gen server without a terminate handler, handle_info handler or any means of trapping exits
-- |
-- | ```purescript
-- | serverName :: ServerName State Unit
-- | serverName = ServerName "some_uuid"
-- |
-- | startLink :: Effect StartLinkResult
-- | startLink = Gen.startLink serverName init
-- |
-- | init :: Effect State
-- | init = pure {}
-- | ```
-- | See also: gen_server:start_link in the OTP docs (roughly)
startLink :: forall state msg. StartName state msg -> Init state msg -> Effect StartLinkResult
startLink name init = buildStartLink name init $ defaultStartLink


-- | Starts a typed gen-server proxy with the supplied ServerName, with the state being the result of the supplied effect
-- | This takes in a builder of optional values which can be overriden (See: StartLinkBuilder)
-- |
-- | ```purescript
-- | serverName :: ServerName State Msg
-- | serverName = ServerName "some_uuid"
-- |
-- | startLink :: Effect StartLinkResult
-- | startLink = Gen.startLink serverName init $ Gen.defaultStartLink { handleInfo: myHandleInfo }
-- |
-- | init :: Effect State
-- | init = pure {}
-- |
-- | handleInfo :: Msg -> State -> Effect (CastResult State)
-- | handleInfo msg state = pure $ CastNoReply state
-- | ```
-- | See also: gen_server:start_link in the OTP docs (roughly)

buildStartLink :: forall state msg. ServerName state msg -> Init state msg -> StartLinkBuilder state msg -> Effect StartLinkResult
buildStartLink (Local name) init builder = foreignToSlr <$> startLinkImpl (tuple2 (atom "local") name) init builder
buildStartLink (Global name) init builder = foreignToSlr <$> startLinkImpl (tuple2 (atom "global") name) init builder
buildStartLink (Via (NativeModuleName m) name) init builder = foreignToSlr <$> startLinkImpl (tuple3 (atom "via") m name) init builder

-- | Creates the default start link options for a gen server
-- | These can be replaced  by modifying the record
defaultStartLink :: forall state msg. StartLinkBuilder state msg
defaultStartLink  = {
    handleInfo : defaultHandleInfo
  , terminate : Nothing
  , trapExit : Nothing
  }


--- Hooks for our supervision tree

foreign import start_from_spec :: forall a. a -> a
foreign import unpackArgsImpl :: forall state msg. Foreign -> Init2Args state msg

-----
-- Actual Gen Callbacks
-----

type Init2Args state msg = { init :: Init state msg, opts :: StartLinkBuilder state msg }

init :: forall state msg. EffectFn1 Foreign Foreign
init = mkEffectFn1 (\args -> init2 $ unpackArgsImpl args)

init2 :: forall state msg. Init2Args state msg  -> Effect Foreign
init2 { init, opts: {  handleInfo, terminate, trapExit } } = do
  _ <- case trapExit of
         Nothing -> pure unit
         Just _  -> enableTrapExitImpl
  pid <- selfImpl

  -- Note: We're discarding the state here, when it comes to registering callbacks/etc
  -- or injecting logging, we'll probably want to keep it so we can use it to populate our StateImpl
  initResponse <- evalStateT init $ GenContext { handleInfo, terminate, trapExit, pid }
  case initResponse of
    InitOk innerState ->
      pure $ unsafeToForeign $ tuple2 (atom "ok") { innerState, context: GenContext { handleInfo, terminate, trapExit, pid } }
    InitOkTimeout innerState timeout ->
      pure $ unsafeToForeign $ tuple3 (atom "ok") { innerState, context: GenContext { handleInfo, terminate, trapExit, pid } } timeout
    InitOkHibernate innerState ->
      pure $ unsafeToForeign $ tuple3 (atom "ok") { innerState, context: GenContext { handleInfo, terminate, trapExit, pid } } (atom "hibernate")
    InitStop terminateReason ->
      pure $ unsafeToForeign $ tuple2 (atom "stop") $ writeTerminateReason terminateReason
    InitIgnore ->
      pure $ unsafeToForeign $ atom "ignore"

handle_call :: forall response state msg. EffectFn3 (StateImpl state msg -> Pid -> Effect (CallResultImpl response state msg))  Pid (StateImpl state msg) (CallResultImpl response state msg)
handle_call = mkEffectFn3 \fn from state -> fn state from

handle_cast :: forall state msg. EffectFn2 (StateImpl state msg -> Effect (CastResultImpl state msg)) (StateImpl state msg) (CastResultImpl state msg)
handle_cast = mkEffectFn2 \fn state -> fn state

handle_info :: forall state msg. EffectFn2 Foreign (StateImpl state msg) (CastResultImpl state msg)
handle_info = mkEffectFn2 \msg state@({ innerState, context: context@(GenContext {  handleInfo, trapExit })}) ->
    let
        mappedMsg = mapInfoMessageImpl trapExit Exit msg
     in
    uncurry dispatchCastResp <$> runStateT (handleInfo mappedMsg innerState) context

terminate :: forall state msg. EffectFn2 Foreign (StateImpl state msg) Atom
terminate = mkEffectFn2 \reason { context: GenContext { terminate:  mt }, innerState } ->
  case mt of
           Just t -> do
             _ <- t (readTerminateReason reason) innerState
             pure $ atom "ok"
           Nothing ->
             pure $ atom "ok"


data InitResult state = InitOk state | InitOkTimeout state Int | InitOkHibernate state | InitStop TerminateReason | InitIgnore
data CallResult response state = CallReply response state | CallReplyHibernate response state | CallStop TerminateReason response state
data CastResult state = CastNoReply state | CastNoReplyHibernate state | CastStop state | CastStopReason TerminateReason state

-- | A default implementation of handleInfo that just ignores any messages received
-- | A  warning will be printed if messages are received
defaultHandleInfo :: forall state msg. msg -> state -> HandleInfo state msg
defaultHandleInfo msg state = do
  lift $ logWarning "Gen server received message, consider looking at startLinkBuilder and supplying a handleInfo function" { msg, state }
  pure $ CastNoReply state



foreign import data CallResultImpl :: Type -> Type -> Type -> Type
foreign import data CastResultImpl :: Type -> Type -> Type

foreign import callReplyImpl :: forall resp state msg. resp -> StateImpl state msg -> CallResultImpl resp state msg
foreign import callReplyHibernateImpl :: forall resp state msg. resp -> StateImpl state msg -> CallResultImpl resp state msg
foreign import callStopImpl :: forall resp state msg. Foreign ->  resp -> StateImpl state msg -> CallResultImpl resp state msg

foreign import castNoReplyImpl :: forall state msg. StateImpl state msg -> CastResultImpl state msg
foreign import castNoReplyHibernateImpl :: forall state msg. StateImpl state msg -> CastResultImpl state msg
foreign import castStopImpl :: forall state msg. Foreign ->  StateImpl state msg -> CastResultImpl state msg
foreign import readTerminateReasonImpl :: Foreign -> TerminateReason -> TerminateReason -> (Foreign -> TerminateReason) -> (Foreign -> TerminateReason) -> TerminateReason


writeTerminateReason :: TerminateReason -> Foreign
writeTerminateReason reason=
  case reason of
       Normal -> unsafeCoerce (atom "normal")
       Shutdown -> unsafeCoerce (atom "shutdown")
       ShutdownWithCustom custom -> unsafeCoerce $ tuple2 (atom "shutdown")  custom
       Custom custom -> unsafeCoerce $ custom


readTerminateReason :: Foreign -> TerminateReason
readTerminateReason f =
  readTerminateReasonImpl f Normal Shutdown ShutdownWithCustom Custom


-- | Defines an effectful call that performs an interaction on the state held by the gen server, and perhaps side-effects
-- | Directly returns the result of the callback provided
-- | ```purescript
-- |
-- | doSomething :: Effect Unit
-- | doSomething = Gen.doCall serverName \state -> pure $ CallResult unit (modifyState state)
-- | ```
-- | See also handle_call and gen_server:call in the OTP docs
doCall :: forall response state msg. ServerName state msg -> (state -> Call response state msg) -> Effect response
doCall name fn = doCallImpl (nativeName name) \genState@{ innerState, context } from ->
  uncurry dispatchCallResp <$> runStateT (fn innerState) context


-- | Defines an effectful cast that performs an interaction on the state held by the gen server
-- | ```purescript
-- | doSomething :: Effect Unit
-- | doSomething = Gen.cast serverName \state -> pure $ CastNoReply $ modifyState state
-- | ```
-- | See also handle_cast and gen_server:cast in the OTP docs
doCast :: forall state msg. ServerName state msg -> (state -> Cast state msg) -> Effect Unit
doCast name fn = doCastImpl (nativeName name) \genState@{ innerState, context } ->
  uncurry dispatchCastResp <$> runStateT (fn innerState) context

stop :: forall state msg. ServerName state msg -> Effect Unit
stop name = stopImpl (nativeName name)


dispatchCallResp :: forall  response state msg. CallResult response state -> GenContext state msg ->  CallResultImpl response state msg
dispatchCallResp resp context =
  case resp of
    CallReply resp newState ->
      callReplyImpl resp $ { innerState: newState, context }
    CallReplyHibernate resp newState ->
      callReplyHibernateImpl resp $ { innerState: newState, context }
    CallStop reason resp newState ->
      callStopImpl (writeTerminateReason reason) resp $ { innerState: newState, context }

dispatchCastResp :: forall  state msg. CastResult state -> GenContext state msg -> CastResultImpl state msg
dispatchCastResp resp context  =
  case resp of
    CastNoReply newState ->
      castNoReplyImpl $ { innerState: newState, context }
    CastNoReplyHibernate newState ->
      castNoReplyHibernateImpl $ { innerState: newState, context }
    CastStop newState ->
      castStopImpl (writeTerminateReason Normal) $ { innerState: newState, context }
    CastStopReason reason newState ->
      castStopImpl (writeTerminateReason reason) $ { innerState: newState, context }
