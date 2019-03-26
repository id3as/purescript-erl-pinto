module Pinto.Gen where

import Effect (Effect)
import Erl.Atom (Atom, atom)
import Pinto (ServerName(..), StartLinkResult)

foreign import callImpl :: forall response state. Atom -> (state -> (CallResult response state)) -> Effect response
foreign import doCallImpl :: forall response state. Atom -> (state -> Effect (CallResult response state)) -> Effect response
foreign import startLinkImpl :: forall state. Atom -> Effect state -> Effect StartLinkResult

startLink :: forall state. ServerName state -> Effect state -> Effect StartLinkResult
startLink (ServerName name) eff =
  startLinkImpl (atom name) eff

data CallResult response state = Reply response state | Stop response state

-- Pure
call :: forall response state. ServerName state -> (state -> (CallResult response state)) -> Effect response
call (ServerName name) fn = callImpl (atom name) fn

-- Impure
doCall :: forall response state. ServerName state -> (state -> Effect (CallResult response state)) -> Effect response
doCall (ServerName name) fn = doCallImpl (atom name) fn

