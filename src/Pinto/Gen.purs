module Pinto.Gen where

import Effect (Effect)
import Erl.Data.Tuple (Tuple2)
import Erl.Atom (Atom, atom)
import Pinto (ServerName(..), StartLinkResult)

foreign import callImpl :: forall response state. Atom -> (state -> (Tuple2 response state)) -> Effect response
foreign import doCallImpl :: forall response state. Atom -> (state -> Effect (Tuple2 response state)) -> Effect response
foreign import startLinkImpl :: forall state. Atom -> Effect state -> Effect StartLinkResult

startLink :: forall state. ServerName state -> Effect state -> Effect StartLinkResult
startLink (ServerName name) eff =
  startLinkImpl (atom name) eff

-- Pure
call :: forall response state. ServerName state -> (state -> (Tuple2 response state)) -> Effect response
call (ServerName name) fn = callImpl (atom name) fn

-- Impure
doCall :: forall response state. ServerName state -> (state -> Effect (Tuple2 response state)) -> Effect response
doCall (ServerName name) fn = doCallImpl (atom name) fn

