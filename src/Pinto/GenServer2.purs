-- | Module representing the gen_server in OTP
-- | See also 'gen_server' in the OTP docs (https://erlang.org/doc/man/gen_server.html)

module Pinto.GenServer2
  ( Action'
  , AllConfig
  , CallFn
  , CallResult'
  , CastFn
  , ContinueFn
  , GSConfig
  , InfoFn
  , InitFn
  , InitResult'
  , OptionalConfig
  , ReturnResult'
  , ServerInstance
  , ServerPid
  , ServerRef
  , ServerType
  , TerminateFn
  , call
  , cast
  , defaultSpec
  , replyTo
  , startLink
  , startLink'
  , stop
  , module CSExports
  ) where

import Prelude

import ConvertableOptions (class ConvertOptionsWithDefaults)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Pinto.GenServer.ContStop (Action(Hibernate, StopNormal), InitResult(InitOk, InitOkHibernate, InitStop, InitIgnore), From, noReply, noReplyWithAction, reply, return, returnWithAction, replyWithAction) as CSExports
import Pinto.GenServer.ContStop (From)
import Pinto.GenServer.ContStop as CS
import Pinto.ProcessT.Internal.Types (class MonadProcessHandled, class MonadProcessTrans)
import Pinto.Types (RegistryInstance, RegistryReference, ShutdownReason, StartLinkResult)

-- In general a gen_server can return continue and stop actions but in practice they
-- very rarely do.  As a result the "full" interface as implemented in Pinto.GenServer.ContStop
-- module is typed over cont and stop messages in addition to state and m (where m is the monad
-- that the gen_server is running under and in turn dictate what messages it can receive).
-- This module provides a cut down interface for gen_servers where cont and stop are both Void.
-- This covers most uses cases seen in the field while cutting down on the type noise...

-- | Simplified CS.ServerPid without cont and stop
type ServerPid :: forall k1 k2. k1 -> k2 -> Type
type ServerPid state m = CS.ServerPid Void Void state m

-- | Simplified CS.ServerType without cont and stop
type ServerType :: forall k1 k2. k1 -> k2 -> Type
type ServerType state m = CS.ServerType Void Void state m

-- | Simplified CS.ServerRef without cont and stop
type ServerRef :: forall k1 k2. k1 -> k2 -> Type
type ServerRef state m = RegistryReference (ServerPid state m) (ServerType state m)

-- | Simplified CS.ServerInstance without cont and stop
type ServerInstance :: forall k1 k2. k1 -> k2 -> Type
type ServerInstance state m = RegistryInstance (ServerPid state m) (ServerType state m)

-- | Simplified CS.Action without cont and stop
type Action' = CS.Action Void Void

-- | The various return values from an init callback
-- | These roughly map onto the tuples in the OTP documentation
type InitResult' state = CS.InitResult Void state

-- | The result of a GenServer.call (handle_call) action
type CallResult' reply state = CS.CallResult reply Void Void state

-- | The result of a GenServer.handle_info or GenServer.handle_cast callback
type ReturnResult' state = CS.ReturnResult Void Void state

type InitFn :: forall k. Type -> (Type -> k) -> k
type InitFn state m = m (InitResult' state)

type InfoFn parsedMsg state m = parsedMsg -> state -> m (ReturnResult' state)
type ContinueFn state m = state -> m (ReturnResult' state)
type CastFn state m = state -> m (ReturnResult' state)
type CallFn reply state m = From reply -> state -> m (CallResult' reply state)
type TerminateFn state m = ShutdownReason -> state -> m Unit

type OptionalConfig parsedMsg state m = CS.OptionalConfig Void Void parsedMsg state m
type AllConfig parsedMsg state m = CS.AllConfig Void Void parsedMsg state m
type GSConfig parsedMsg state m = CS.GSConfig Void Void parsedMsg state m

startLink
  :: forall appMsg parsedMsg state m mState
   . MonadProcessHandled m parsedMsg
  => MonadProcessTrans m mState appMsg parsedMsg
  => GSConfig parsedMsg state m
  -> Effect (StartLinkResult (ServerPid state m))
startLink = CS.startLink

startLink'
  :: forall providedConfig appMsg parsedMsg state m mState
   . MonadProcessHandled m parsedMsg
  => MonadProcessTrans m mState appMsg parsedMsg
  => ConvertOptionsWithDefaults CS.OptionToMaybe { | OptionalConfig parsedMsg state m } { | providedConfig } { | AllConfig parsedMsg state m }
  => { | providedConfig }
  -> Effect (StartLinkResult (ServerPid state m))
startLink' = CS.startLink'

call
  :: forall reply appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m -- TODO - why is the monad constraint needed?
  => ServerRef state m
  -> CallFn reply state m
  -> Effect reply
call = CS.call

cast
  :: forall appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m
  => ServerRef state m
  -> CastFn state m
  -> Effect Unit
cast = CS.cast

stop
  :: forall appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Monad m
  => ServerRef state m
  -> Effect Unit
stop = CS.stop

replyTo
  :: forall reply m
   . MonadEffect m
  => From reply
  -> reply
  -> m Unit
replyTo = CS.replyTo

defaultSpec
  :: forall parsedMsg appMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => InitFn state m
  -> GSConfig parsedMsg state m
defaultSpec = CS.defaultSpec
