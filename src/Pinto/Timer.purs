module Pinto.Timer 
  ( sendEvery,
    sendAfter,
    cancel,
    TimerRef ) 
    where

import Prelude

import Effect (Effect)
import Erl.Process.Raw (Pid)
import Pinto (ServerName)

newtype TimerRef = TimerRef Pid

foreign import sendEvery_ :: forall state. (Pid -> TimerRef) -> (ServerName state) -> Int -> (state -> Effect state) -> Effect TimerRef
foreign import sendAfter_ :: forall state. (Pid -> TimerRef) -> (ServerName state) -> Int -> (state -> Effect state) -> Effect TimerRef
foreign import cancel_ :: Pid -> Effect Unit

sendEvery :: forall state. ServerName state -> Int -> (state -> Effect state) -> Effect TimerRef
sendEvery = sendEvery_ TimerRef

sendAfter :: forall state. ServerName state -> Int -> (state -> Effect state) -> Effect TimerRef
sendAfter = sendAfter_ TimerRef

cancel :: TimerRef -> Effect Unit
cancel (TimerRef pid) = cancel_ pid
