-- | Module roughly representing interactions with the 'gen_server'
-- | See also 'gen_server' in the OTP docs
module Pinto.Gen ( startLink
                 , buildStartLink
                 , StartLinkBuilder(..)
                 , defaultStartLink
                 , stop
                 , CallResult(..)
                 , CastResult(..)
                 , doCall
                 , doCast
                 , defaultHandleInfo
                 , whereIs
                 , monitor
                 , self
                 , ExitMessage(..)
                 )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2, tuple3, Tuple2(..))
import Erl.ModuleName (NativeModuleName(..))
import Erl.Process (Process(..), runProcess)
import Erl.Process.Raw (Pid)
import Erl.Atom (Atom, atom)
import Foreign (Foreign, unsafeToForeign)
import Pinto (ServerName(..), StartLinkResult, TerminateReason(..))
import Control.Monad.State (State, StateT)
import Pinto.MessageRouting as MR
import Pinto.Monitor as Monitor
import Pinto.Sup (foreignToSlr)
import Unsafe.Coerce (unsafeCoerce)

data ExitMessage = Exit Pid Foreign

foreign import enableTrapExitImpl :: Effect Unit
foreign import doCallImpl :: forall name state msg response. name -> ((StateImpl state msg) -> Pid -> Effect (CallResultImpl response state msg)) -> Effect response
foreign import doCastImpl :: forall name state msg. name -> ((StateImpl state msg) -> Effect (CastResultImpl state msg)) -> Effect Unit
foreign import stopImpl :: forall name. name -> Effect Unit
foreign import startLinkImpl :: forall name state msg. name -> Effect state -> StartLinkBuilder state msg -> Effect Foreign
foreign import selfImpl :: forall msg serverName. serverName -> Effect (Process msg)
foreign import whereIsImpl :: forall msg name. name -> (Process msg -> Maybe (Process msg)) -> (Maybe (Process msg)) -> Effect (Maybe (Process msg))
foreign import logWarning :: forall obj. String -> obj -> Effect Unit

nativeName :: forall state msg. ServerName state msg -> Foreign
nativeName (Local name) = unsafeToForeign $ name
nativeName (Global name) = unsafeToForeign $ tuple2 (atom "global") name
nativeName (Via (NativeModuleName m) name) = unsafeToForeign $ tuple3 (atom "via") m name


-- | Gets the pid for this gen server
-- | This can only be called by the process itself because it is guaranteed to succeed and doesn't need
-- | a Maybe around it, calls external to the process will fail an assertion to verify that you're not being naughty
self :: forall state msg. ServerName state msg -> Effect (Process msg)
self serverName = selfImpl (nativeName serverName)

-- | Gets the pid of this gen server (if running)
-- | This is designed to be called from external agents and therefore might fail, hence the Maybe 
whereIs :: forall state msg. ServerName state msg -> Effect (Maybe (Process msg))
whereIs serverName = whereIsImpl (nativeName serverName) Just Nothing

-- | Short cut for monitoring a gen server via Pinto.Monitor
monitor :: forall state msg. ServerName state msg -> (Monitor.MonitorMsg -> Effect Unit) -> Effect Unit -> Effect (Maybe (MR.RouterRef Monitor.MonitorRef))
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
    handleInfo :: msg -> state -> Effect (CastResult state)

    -- | A callback to be invoked when this gen server terminates
  , terminate :: Maybe  (TerminateReason -> state -> Effect Unit)

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
startLink :: forall state msg. ServerName state msg -> Effect state -> Effect StartLinkResult
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

buildStartLink :: forall state msg. ServerName state msg -> Effect state -> StartLinkBuilder state msg -> Effect StartLinkResult
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

-----
-- Actual Gen Callbacks
-----

type StateImpl state msg = { innerState :: state
                           , handleInfo :: msg -> state -> Effect (CastResult state)
                           , terminate :: Maybe  (TerminateReason -> state -> Effect Unit)
                           , trapExit :: Maybe (ExitMessage -> msg)
                           }

init :: forall state msg. { init :: Effect state, opts :: StartLinkBuilder state msg } -> Effect (Tuple2 Atom (StateImpl state msg))
init { init, opts: {  handleInfo, terminate, trapExit } } = do
  _ <- case trapExit of
         Nothing -> pure unit
         Just _  -> enableTrapExitImpl
  innerState <- init
  pure $ tuple2 (atom "ok") $ { innerState, handleInfo, terminate, trapExit } 

handle_call :: forall response state msg. (StateImpl state msg -> Effect (CallResultImpl response state msg)) -> Pid -> StateImpl state msg -> Effect (CallResultImpl response state msg)
handle_call fn _from state = fn state

handle_cast :: forall state msg. (StateImpl state msg -> Effect (CastResultImpl state msg)) -> StateImpl state msg -> Effect (CastResultImpl state msg)
handle_cast fn state = fn state

handle_info :: forall state msg. Foreign -> StateImpl state msg -> Effect (CastResultImpl state msg)
handle_info msg state@{ innerState, handleInfo } =
  -- TODO: Check if this is an exit message and convert accordingly
  dispatchCastResp state <$> handleInfo (unsafeCoerce msg) innerState

terminate :: forall state msg. Foreign -> StateImpl state msg -> Effect Atom 
terminate reason { terminate:  mt, innerState } = 
  case mt of
     Just t -> do
       _ <- t (readTerminateReason reason) innerState
       pure $ atom "ok"
     Nothing ->
       pure $ atom "ok"

--handle_info({'EXIT', FromPid, Reason}, StateImpl = #state_impl {
--                                                      trap_exit = TrapExitMsg
--                                                     }) ->
--  ConvertedMessage = TrapExitMsg({exit, FromPid,  Reason}),
--  pinto_gen@ps:handleExit(ConvertedMessage, StateImpl)
--  dispatch_cast_response(((HandleInfo())(State))(), StateImpl);
--
--handle_info(Msg, StateImpl = #state_impl { state = State, handle_info = HandleInfo}) ->
--  dispatch_cast_response(((HandleInfo(Msg))(State))(), StateImpl).

-----


data CallResult response state = CallReply response state | CallReplyHibernate response state | CallStop TerminateReason response state
data CastResult state = CastNoReply state | CastNoReplyHibernate state | CastStop state | CastStopReason TerminateReason state

-- | A default implementation of handleInfo that just ignores any messages received
-- | A  warning will be printed if messages are received
defaultHandleInfo :: forall state msg. msg -> state -> Effect (CastResult state)
defaultHandleInfo msg state = do
  logWarning "Gen server received message, consider looking at startLinkBuilder and supplying a handleInfo function" { msg, state }
  pure $ CastNoReply state


-- | The type of any effectful callback into this gen server
type GenResultT response msg state = StateT (StateImpl state msg) Effect response

-- | The type of any pure callback into this gen server
type GenResult response msg state = State (StateImpl state msg) response


foreign import data CallResultImpl :: Type -> Type -> Type -> Type
foreign import data CastResultImpl :: Type -> Type -> Type

foreign import callReplyImpl :: forall resp state msg. resp -> StateImpl state msg -> CallResultImpl resp state msg
foreign import callReplyHibernateImpl :: forall resp state msg. resp -> StateImpl state msg -> CallResultImpl resp state msg
foreign import callStopImpl :: forall resp state msg. Foreign ->  resp -> StateImpl state msg -> CallResultImpl resp state msg

foreign import castNoReplyImpl :: forall state msg. StateImpl state msg -> CastResultImpl state msg
foreign import castNoReplyHibernateImpl :: forall state msg. StateImpl state msg -> CastResultImpl state msg
foreign import castStopImpl :: forall state msg. Foreign ->  StateImpl state msg -> CastResultImpl state msg


writeTerminateReason :: TerminateReason -> Foreign
writeTerminateReason reason=
  case reason of
       Normal -> unsafeCoerce (atom "normal")
       Shutdown -> unsafeCoerce (atom "shutdown") 
       ShutdownWithCustom custom -> unsafeCoerce $ tuple2 (atom "shutdown")  custom
       Custom custom -> unsafeCoerce $ custom

foreign import readTerminateReasonImpl :: Foreign -> TerminateReason -> TerminateReason -> (Foreign -> TerminateReason) -> (Foreign -> TerminateReason) -> TerminateReason

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
doCall :: forall response state msg. ServerName state msg -> (state -> Effect (CallResult response state)) -> Effect response
doCall name fn = doCallImpl (nativeName name) \genState@{ innerState } from -> 
  dispatchCallResp genState <$> fn innerState

-- | Defines an effectful cast that performs an interaction on the state held by the gen server
-- | ```purescript 
-- | doSomething :: Effect Unit
-- | doSomething = Gen.cast serverName \state -> pure $ CastNoReply $ modifyState state
-- | ```
-- | See also handle_cast and gen_server:cast in the OTP docs
doCast :: forall state msg. ServerName state msg -> (state -> Effect (CastResult state)) -> Effect Unit
doCast name fn = doCastImpl (nativeName name) \genState@{ innerState } -> 
  dispatchCastResp genState <$> fn innerState

stop :: forall state msg. ServerName state msg -> Effect Unit
stop name = stopImpl (nativeName name)


dispatchCallResp :: forall  response state msg. StateImpl state msg -> CallResult response state -> CallResultImpl response state msg
dispatchCallResp genState resp =
  case resp of
    CallReply resp newState ->
      callReplyImpl resp genState { innerState = newState }
    CallReplyHibernate resp newState ->
      callReplyHibernateImpl resp genState { innerState = newState }
    CallStop reason resp newState ->
      callStopImpl (writeTerminateReason reason) resp genState { innerState = newState }

dispatchCastResp :: forall  state msg. StateImpl state msg -> CastResult state -> CastResultImpl state msg
dispatchCastResp genState resp =
  case resp of
    CastNoReply newState ->
      castNoReplyImpl $ genState { innerState = newState }
    CastNoReplyHibernate newState ->
      castNoReplyHibernateImpl $ genState { innerState = newState }
    CastStop newState ->
      castStopImpl (writeTerminateReason Normal) $ genState { innerState = newState }
    CastStopReason reason newState ->
      castStopImpl (writeTerminateReason reason) $ genState { innerState =  newState }


