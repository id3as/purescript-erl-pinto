-- | Module roughly representing interactions with the 'gen_server' 
-- | See also 'gen_server' in the OTP docs
module Pinto.Gen ( startLink
                 , CallResult(..)
                 , CastResult(..)
                 , call
                 , doCall
                 , cast
                 , doCast
                 , defaultHandleInfo
                 , registerExternalMapping
                 ) 
  where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Pinto (ServerName(..), StartLinkResult)
import Data.Maybe (Maybe(..))

foreign import callImpl :: forall response state. Atom -> (state -> (CallResult response state)) -> Effect response
foreign import doCallImpl :: forall response state. Atom -> (state -> Effect (CallResult response state)) -> Effect response
foreign import castImpl :: forall state. Atom -> (state -> (CastResult state)) -> Effect Unit
foreign import doCastImpl :: forall state. Atom -> (state -> Effect (CastResult state)) -> Effect Unit
foreign import startLinkImpl :: forall state msg. Atom -> Effect state -> (msg -> state -> Effect state) -> Effect StartLinkResult
foreign import registerExternalMappingImpl :: forall state externalMsg msg. Atom -> (externalMsg -> Maybe msg) -> Effect Unit

-- These imports are just so we don't get warnings
foreign import code_change :: forall a. a -> a -> a -> a
foreign import handle_call :: forall a. a -> a -> a -> a
foreign import handle_cast :: forall a. a -> a -> a -> a
foreign import handle_info :: forall a. a -> a -> a
foreign import init :: forall a. a -> a
foreign import terminate :: forall a. a -> a -> a
foreign import start_from_spec :: forall a. a -> a


-- | Adds a (presumably) native Erlang function into the gen server to map external messages into types that this
-- | gen server actually understands
registerExternalMapping :: forall state externalMsg msg. ServerName state msg -> (externalMsg -> Maybe msg) -> Effect Unit
registerExternalMapping (ServerName name) = registerExternalMappingImpl (atom name)


-- | Starts a typed gen-server proxy with the supplied ServerName, with the state being the result of the supplied effect
-- |
-- | ```purescript
-- | serverName :: ServerName State Unit
-- | serverName = ServerName "some_uuid"
-- | 
-- | startLink :: Effect StartLinkResult
-- | startLink = Gen.startLink serverName init defaultHandleInfo
-- | 
-- | init :: Effect State
-- | init = pure {}
-- | ```
-- | See also: gen_server:start_link in the OTP docs (roughly)
startLink :: forall state msg. ServerName state msg -> Effect state -> (msg -> state -> Effect state) -> Effect StartLinkResult
startLink (ServerName name) = startLinkImpl (atom name)

data CallResult response state = CallReply response state | CallReplyHibernate response state | CallStop response state
data CastResult state = CastNoReply state | CastNoReplyHibernate state | CastStop state

-- | A default implementation of handleInfo that just ignores any messages received
defaultHandleInfo :: forall state msg. msg -> state -> Effect state
defaultHandleInfo msg state = pure state

-- | Defines a "pure" "call" that performs an interaction on the state held by the gen server, but with no other side effects
-- | Directly returns the result of the callback provided
-- | ```purescript
-- | 
-- | doSomething :: Effect Unit
-- | doSomething = Gen.call serverName \state -> CallReply unit (modifyState state)
-- | ```
-- | See also handle_call and gen_server:call in the OTP docs
call :: forall response state msg. ServerName state msg -> (state -> (CallResult response state)) -> Effect response
call (ServerName name) fn = callImpl (atom name) fn

-- | Defines an effectful call that performs an interaction on the state held by the gen server, and perhaps side-effects
-- | Directly returns the result of the callback provided
-- | ```purescript
-- | 
-- | doSomething :: Effect Unit
-- | doSomething = Gen.doCall serverName \state -> pure $ CallReply unit (modifyState state)
-- | ```
-- | See also handle_call and gen_server:call in the OTP docs
doCall :: forall response state msg. ServerName state msg -> (state -> Effect (CallResult response state)) -> Effect response
doCall (ServerName name) fn = doCallImpl (atom name) fn

-- | Defines an "pure" cast that performs an interaction on the state held by the gen server
-- | ```purescript
-- | doSomething :: Effect Unit
-- | doSomething = Gen.cast serverName \state -> CastNoReply $ modifyState state
-- | ```
-- | See also handle_cast and gen_server:cast in the OTP docs
cast :: forall state msg. ServerName state msg -> (state -> (CastResult state)) -> Effect Unit
cast (ServerName name) fn = castImpl (atom name) fn

-- | Defines an effectful cast that performs an interaction on the state held by the gen server
-- | ```purescript
-- | doSomething :: Effect Unit
-- | doSomething = Gen.cast serverName \state -> pure $ CastNoReply $ modifyState state
-- | ```
-- | See also handle_cast and gen_server:cast in the OTP docs
doCast :: forall state msg. ServerName state msg -> (state -> Effect (CastResult state)) -> Effect Unit
doCast (ServerName name) fn = doCastImpl (atom name) fn
