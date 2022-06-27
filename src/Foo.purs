module Foo
where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Erl.Kernel.Erlang as Kernel
import Run (EFFECT, Run, Step(..), interpret, liftEffect, match, on, runAccumPure, runBaseEffect, runCont, send)
import Run as Run
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

data TalkF a
  = Speak String a
  | Listen (String -> a)

derive instance functorTalkF :: Functor TalkF

type TALK r = (talk :: TalkF | r)

_talk = Proxy :: Proxy "talk"

speak :: forall r. String -> Run (TALK + r) Unit
speak str = Run.lift _talk (Speak str unit)

listen :: forall r. Run (TALK + r) String
listen = Run.lift _talk (Listen identity)

handleTalk :: forall r. TalkF ~> Run (EFFECT + r)
handleTalk = case _ of
  Speak str next -> do
    liftEffect $ Console.log str
    pure next
  Listen reply -> do
    pure (reply "I am Groot")

runTalk
  :: forall r
   . Run (EFFECT + TALK + r)
       ~> Run (EFFECT + r)
runTalk = interpret (on _talk handleTalk send)

---

type IsThereMore = Boolean
type Bill = Int

data Food = Pizza | Chizburger

data DinnerF a
  = Eat Food (IsThereMore -> a)
  | CheckPlease (Bill -> a)

derive instance functorDinnerF :: Functor DinnerF

type DINNER r = (dinner :: DinnerF | r)

_dinner = Proxy :: Proxy "dinner"

eat :: forall r. Food -> Run (DINNER + r) IsThereMore
eat food = Run.lift _dinner (Eat food identity)

checkPlease :: forall r. Run (DINNER + r) Bill
checkPlease = Run.lift _dinner (CheckPlease identity)

type Tally = { stock :: Int, bill :: Bill }

handleDinner :: forall a. Tally -> DinnerF a -> Tuple Tally a
handleDinner tally = case _ of
  Eat _ reply
    | tally.stock > 0 ->
        let
          tally' = { stock: tally.stock - 1, bill: tally.bill + 1 }
        in
          Tuple tally' (reply true)
    | otherwise ->
        Tuple tally (reply false)
  CheckPlease reply ->
    Tuple tally (reply tally.bill)

runDinnerPure :: forall r a. Tally -> Run (DINNER + r) a -> Run r (Tuple Bill a)
runDinnerPure = runAccumPure
  (\tally -> on _dinner (Loop <<< handleDinner tally) Done)
  (\tally a -> Tuple tally.bill a)

---

type LovelyEvening r = (TALK + DINNER + r)

dinnerTime :: forall r. Run (LovelyEvening r) Unit
dinnerTime = do
  speak "I'm famished!"
  isThereMore <- eat Pizza
  if isThereMore then dinnerTime
  else do
    bill <- checkPlease
    speak $ "Outrageous!" <> show bill

program2 :: forall r. Run (EFFECT + DINNER + r) Unit
program2 = dinnerTime # runTalk

program3 :: forall r. Run (EFFECT + r) (Tuple Bill Unit)
program3 = program2 # runDinnerPure { stock: 10, bill: 0 }

main :: Effect (Tuple Bill Unit)
main = runBaseEffect program3
