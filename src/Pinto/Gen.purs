module Pinto.Gen ( startLink
                 , CallResult(..)
                 , CastResult(..)
                 , call
                 , doCall
                 , cast
                 , doCast
                 ) 
  where

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Pinto (ServerName(..), StartLinkResult)
import Prelude (Unit)

foreign import callImpl :: forall response state. Atom -> (state -> (CallResult response state)) -> Effect response
foreign import doCallImpl :: forall response state. Atom -> (state -> Effect (CallResult response state)) -> Effect response
foreign import castImpl :: forall state. Atom -> (state -> (CastResult state)) -> Effect Unit
foreign import doCastImpl :: forall state. Atom -> (state -> Effect (CastResult state)) -> Effect Unit
foreign import startLinkImpl :: forall state. Atom -> Effect state -> Effect StartLinkResult

-- | Starts a typed gen-server proxy with the supplied ServerName, with the state being the result of the supplied effect
-- |
-- | ```purescript
-- | serverName :: ServerName State
-- | serverName = ServerName "some_uuid"
-- | 
-- | startLink :: Effect StartLinkResults
-- | startLink = Gen.startLink serverName init
-- | 
-- | init :: Effect State
-- | init = pure {}
-- | ```
-- | See also: gen_server:start_link in the OTP docs
startLink :: forall state. ServerName state -> Effect state -> Effect StartLinkResult
startLink (ServerName name) eff =
  startLinkImpl (atom name) eff

data CallResult response state = CallReply response state | CallReplyHibernate response state | CallStop response state
data CastResult state = CastNoReply state | CastNoReplyHibernate state | CastStop state

-- | Defines a "pure" "call" that performs an interaction on the state held by the gen server, but with no other side effects
-- | Directly returns the result of the callback provided
-- | ```purescript
-- | 
-- | doSomething :: Effect Unit
-- | doSomething = Gen.call serverName \state -> CallReply unit (modifyState state)
-- | ```
-- | See also handle_call and gen_server:call in the OTP docs
call :: forall response state. ServerName state -> (state -> (CallResult response state)) -> Effect response
call (ServerName name) fn = callImpl (atom name) fn

-- | Defines an effectful call that performs an interaction on the state held by the gen server, and perhaps side-effects
-- | Directly returns the result of the callback provided
-- | ```purescript
-- | 
-- | doSomething :: Effect Unit
-- | doSomething = Gen.doCall serverName \state -> pure $ CallReply unit (modifyState state)
-- | ```
-- | See also handle_call and gen_server:call in the OTP docs
doCall :: forall response state. ServerName state -> (state -> Effect (CallResult response state)) -> Effect response
doCall (ServerName name) fn = doCallImpl (atom name) fn

-- | Defines an "pure" cast that performs an interaction on the state held by the gen server
-- | ```purescript
-- | doSomething :: Effect Unit
-- | doSomething = Gen.cast serverName \state -> CastNoReply $ modifyState state
-- | ```
-- | See also handle_cast and gen_server:cast in the OTP docs
cast :: forall state. ServerName state -> (state -> (CastResult state)) -> Effect Unit
cast (ServerName name) fn = castImpl (atom name) fn

-- | Defines an effectful cast that performs an interaction on the state held by the gen server
-- | ```purescript
-- | doSomething :: Effect Unit
-- | doSomething = Gen.cast serverName \state -> pure $ CastNoReply $ modifyState state
-- | ```
-- | See also handle_cast and gen_server:cast in the OTP docs
doCast :: forall state. ServerName state -> (state -> Effect (CastResult state)) -> Effect Unit
doCast (ServerName name) fn = doCastImpl (atom name) fn
