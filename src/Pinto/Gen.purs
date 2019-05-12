module Pinto.Gen where

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Pinto (ServerName(..), StartLinkResult)
import Prelude (Unit)

foreign import callImpl :: forall response state. Atom -> (state -> (CallResult response state)) -> Effect response
foreign import doCallImpl :: forall response state. Atom -> (state -> Effect (CallResult response state)) -> Effect response
foreign import castImpl :: forall state. Atom -> (state -> (CastResult state)) -> Effect Unit
foreign import doCastImpl :: forall state. Atom -> (state -> Effect (CastResult state)) -> Effect Unit
foreign import startLinkImpl :: forall state. Atom -> Effect state -> Effect StartLinkResult

startLink :: forall state. ServerName state -> Effect state -> Effect StartLinkResult
startLink (ServerName name) eff =
  startLinkImpl (atom name) eff

data CallResult response state = CallReply response state | CallStop response state
data CastResult state = CastNoReply state | CastStop state

-- Pure
call :: forall response state. ServerName state -> (state -> (CallResult response state)) -> Effect response
call (ServerName name) fn = callImpl (atom name) fn

-- Impure
doCall :: forall response state. ServerName state -> (state -> Effect (CallResult response state)) -> Effect response
doCall (ServerName name) fn = doCallImpl (atom name) fn


-- Pure
cast :: forall state. ServerName state -> (state -> (CastResult state)) -> Effect Unit
cast (ServerName name) fn = castImpl (atom name) fn

-- Impure
doCast :: forall state. ServerName state -> (state -> Effect (CastResult state)) -> Effect Unit
doCast (ServerName name) fn = doCastImpl (atom name) fn

