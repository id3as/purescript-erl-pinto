module Foo
where

import Prelude

import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Functor.Variant (VariantF, match, on, prj)
import Data.Lens (Lens', view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, expand, inj)
import Debug (spy, traceM)
import Effect (Effect)
import Effect.Console as Console
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process.Raw as Raw
import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder, build)
import Record.Builder as RecordBuilder
import Run (EFFECT, Run, Step(..), interpret, liftEffect, on, runAccumCont, runAccumPure, runBaseEffect, runCont, send)
import Run (EFFECT, Run, Step(..), interpret, liftEffect, on, runAccumPure, runBaseEffect, send)
import Run as Run
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

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
    speak $ "Outrageous! " <> show bill

program2 :: forall r. Run (EFFECT + DINNER + r) Unit
program2 = dinnerTime # runTalk

program3 :: forall r. Run (EFFECT + r) (Tuple Bill Unit)
program3 = program2 # runDinnerPure { stock: 10, bill: 0 }

data ProcessF msg a
  = Receive (msg -> a)

derive instance Functor (ProcessF msg)

type MyProcessM r = (myProcessM :: ProcessF)
_myProcessM = Proxy :: Proxy "myProcessM"

myReceive :: forall msg r. (ProcessF msg) ~> Run (EFFECT + r)
myReceive = case _ of
  Receive reply -> do
    msg <- liftEffect $ Raw.receive
    pure (reply msg)

--------------------------------------
-- | ProcessM...
data ProcessM msg a
  = ProcessMParse a

derive instance Functor (ProcessM msg)

type PROCESSM msg r = (z_processM :: ProcessM msg | r)

_processM = Proxy :: Proxy "z_processM"

-- todo - would like to remove maybe here...
processMParse :: forall msg r r1. Foreign -> Run ((PROCESSM msg) + r) (Maybe msg)
processMParse fgn = Run.lift _processM (ProcessMParse $ Just $ unsafeCoerce fgn)

--------------------------------------
-- | Monitor...
newtype MonitorRef = MonitorRef Unit
newtype Pid = Pid Unit

data MonitorF a
  = Monitor Pid (MonitorRef -> a)
  | Demonitor MonitorRef a
  | MonitorParse a

data MonitorMsg

derive instance Functor MonitorF

type MONITOR r = (monitor :: MonitorF | r)

_monitor = Proxy :: Proxy "monitor"

monitor :: forall r. Pid -> Run (MONITOR + r) MonitorRef
monitor pid = Run.lift _monitor (Monitor pid identity)

demonitor :: forall r. MonitorRef -> Run (MONITOR + r) Unit
demonitor ref = Run.lift _monitor (Demonitor ref unit)

monitorParse :: forall r. Foreign -> Run (MONITOR + r) (Maybe MonitorMsg)
monitorParse fgn = Run.lift _monitor (MonitorParse $ parser fgn)
 where
   parser :: Foreign -> Maybe MonitorMsg
   parser fgn = do
     if (unsafeCoerce fgn :: Int) == 1 then do
       traceM "monitor parse!"
       Just (unsafeCoerce "it was a monitor")
     else
       Nothing

--------------------------------------
-- | TrapExit...
data TrapExitF a
  = TrapExitParse a

data TrapExitMsg

derive instance Functor TrapExitF

type TRAPEXIT r = (trapExit :: TrapExitF | r)

_trapExit = Proxy :: Proxy "trapExit"

trapExitParse :: forall r r1. Foreign -> Run (TRAPEXIT + r) (Maybe TrapExitMsg)
trapExitParse fgn = Run.lift _trapExit (TrapExitParse $ parser fgn)
 where
   parser :: Foreign -> Maybe TrapExitMsg
   parser fgn =
     if (unsafeCoerce fgn :: Int) == 2 then do
       traceM "trap exit parse!"
       Just (unsafeCoerce "it was a trap exit")
     else
       Nothing

--------------------------------------
-- | Parse...

class ParseType functorType r messageType | functorType -> messageType r where
  parseType :: Proxy functorType -> Foreign -> Run r (Maybe messageType)

instance ParseType MonitorF (MONITOR ()) MonitorMsg where
  parseType _ fgn = monitorParse fgn

instance ParseType TrapExitF (TRAPEXIT ()) TrapExitMsg where
  parseType _ fgn = trapExitParse fgn

instance ParseType (ProcessM msg) (PROCESSM msg ()) msg where
  parseType _ fgn = processMParse fgn

class ParseResult :: Row (Type -> Type) -> Row Type -> Constraint
class ParseResult stack result | stack -> result where
  parseIt :: Proxy stack -> Foreign -> Run stack (Variant result)

instance
  ( RowToList stack stackL
  , ParseResultRL stackL stack result ) =>
  ParseResult stack result where
  parseIt _  = parseItRL (Proxy :: _ stackL)

class ParseResultRL :: RowList (Type -> Type) -> Row (Type -> Type) -> Row Type -> Constraint
class ParseResultRL stackL stack result | stackL -> stack result where
  parseItRL :: Proxy stackL -> Foreign -> Run stack (Variant result)

instance ParseResultRL RL.Nil () () where
  parseItRL _ _fgn = unsafeCrashWith "WTF" -- we shouldn't get here - or this is processM? If processM, we'd need a newtype for run to squirrel the message type in...

instance
  ( ParseResultRL stackLTail stackTail resultTail
  , ParseType functorType functorStackEntry parseType
  , Row.Cons sym parseType resultTail result
  , Row.Cons sym functorType stackTail stack
  , Row.Union resultTail t1 result
  , Row.Union stackTail t2 stack
  , Row.Union functorStackEntry t3 stack
  , IsSymbol sym
  ) =>
  ParseResultRL (RL.Cons sym functorType stackLTail) stack result where
  parseItRL _ fgn = do
    res <- Run.expand $ parseType (Proxy :: _ functorType) fgn
    case res of
      Just val ->
        pure $ inj (Proxy :: _ sym) val
      Nothing -> do
        Run.expand $ expand <$> parseItRL (Proxy :: _ stackLTail) fgn

parse :: forall stack result.
         ParseResult stack result =>
         Foreign -> Run (stack) (Variant result)
parse fgn = parseIt (Proxy :: _ stack) fgn

--------------------------------------
-- | Execution...

class ProcessHandler :: Row (Type -> Type) -> Row Type -> Constraint
class ProcessHandler stack acc where
  handle :: Record acc -> VariantF stack (Record acc -> (Effect (Record acc))) -> Effect (Record acc)

instance
  ( RowToList stack stackL
  , ProcessHandlerRL stackL stack acc) =>
  ProcessHandler stack acc where
    handle = handleL (Proxy :: _ stackL)

class ProcessHandlerRL :: RowList (Type -> Type) -> Row (Type -> Type) -> Row Type -> Constraint
class ProcessHandlerRL stackL stack acc | stackL -> stack acc where
  handleL :: Proxy stackL -> Record acc -> VariantF stack (Record acc -> (Effect (Record acc))) -> Effect (Record acc)

instance ProcessHandlerRL RL.Nil () acc where
  handleL _ _ _ = unsafeCrashWith "sad"

instance
  ( ProcessHandlerRL stackLTail stackTail acc
  , Row.Cons sym functorType stackTail stack
  , FunctorHandler functorType functorAccType
  , IsSymbol sym
  , Row.Cons sym functorAccType t1 acc
  , Functor functorType
  ) =>
  ProcessHandlerRL (RL.Cons sym functorType stackLTail) stack acc where
  handleL _ acc variant = do
    let
      fn :: functorType (Record acc -> Effect (Record acc)) -> Effect (Record acc)
      fn functorInstance = do
        let
          typeAcc :: functorAccType
          typeAcc = Record.get (Proxy :: _ sym) acc
          updateAcc :: (Record acc -> Effect (Record acc) ) -> (functorAccType -> Effect functorAccType)
          updateAcc fn = \inner -> do
            Record.get (Proxy :: _ sym) <$> (fn $ Record.set (Proxy :: _ sym) inner acc)

        functorAcc' :: functorAccType <- handleFunctor typeAcc $ map updateAcc functorInstance
        pure $ Record.set (Proxy :: _ sym) functorAcc' acc

      recurse :: VariantF stackTail (Record acc -> (Effect (Record acc))) -> Effect (Record acc)
      recurse = handleL (Proxy :: _ stackLTail) acc
    on (Proxy :: _ sym) fn recurse variant

class FunctorHandler :: (Type -> Type) -> Type -> Constraint
class FunctorHandler functorType functorAcc | functorType -> functorAcc where
  handleFunctor :: functorAcc -> functorType (functorAcc -> Effect functorAcc) -> Effect functorAcc

instance FunctorHandler (ProcessM msg) Unit where
  handleFunctor acc (ProcessMParse cb) = cb acc

instance FunctorHandler TrapExitF Unit where
  handleFunctor acc (TrapExitParse cb) = cb acc

instance FunctorHandler MonitorF  (Map MonitorRef Unit) where
  handleFunctor acc (Monitor pid cb) = do
    let
      doTheMonitor :: Pid -> Effect MonitorRef
      doTheMonitor _ = pure $ (unsafeCoerce 1 :: MonitorRef)
    ref <- doTheMonitor pid
    cb ref $ Map.insert ref unit acc
  handleFunctor acc (Demonitor ref cb) = do
    let
      doTheDeMonitor :: MonitorRef -> Effect Unit
      doTheDeMonitor _ = pure unit
    doTheDeMonitor ref
    cb acc
  handleFunctor acc (MonitorParse cb) = do
    cb acc

class ProcessDefault :: Row (Type -> Type) -> Row Type -> Constraint
class ProcessDefault stack defaultRow where
  def :: Proxy stack -> Record defaultRow

instance
  ( RowToList stack stackL
  , ProcessDefaultRL stackL defaultRow
  ) =>
  ProcessDefault stack defaultRow where
  def _ = build (defL (Proxy :: _ stackL)) {}

class ProcessDefaultRL :: RowList (Type -> Type) -> Row Type -> Constraint
class ProcessDefaultRL stack defaultRow | stack -> defaultRow where
  defL :: Proxy stack -> Builder {} {| defaultRow}

instance ProcessDefaultRL RL.Nil () where
  defL _ = identity

instance
  ( ProcessDefaultRL stackTail defaultRowTail
  , FunctorDefault functorType functorDefaultType
  , Row.Cons sym functorDefaultType defaultRowTail defaultRow
  , Row.Lacks sym defaultRowTail
  , IsSymbol sym
  ) =>
  ProcessDefaultRL (RL.Cons sym functorType stackTail) defaultRow where
  defL _ = RecordBuilder.insert (Proxy :: _ sym) (functorDef (Proxy :: _ functorType)) <<< defL (Proxy :: _ stackTail)

class FunctorDefault :: (Type -> Type) -> Type -> Constraint
class FunctorDefault functorType functorDefaultType | functorType -> functorDefaultType where
  functorDef :: Proxy functorType -> functorDefaultType

instance FunctorDefault (ProcessM msg) Unit where
  functorDef _ = unit

instance FunctorDefault TrapExitF Unit where
  functorDef _ = unit

instance FunctorDefault MonitorF (Map MonitorRef Unit) where
  functorDef _ = Map.empty

--------------------------------------
-- | Program...
data AppMsg

type MyStack = (MONITOR + TRAPEXIT + PROCESSM AppMsg + ())
myGenServer :: forall r. Run MyStack Unit
myGenServer = do
  let
    pid = Pid unit
  ref <- monitor pid
  let
    msg1 = unsafeCoerce 1 :: Foreign
    msg2 = unsafeCoerce 2 :: Foreign
    msg3 = unsafeCoerce 3 :: Foreign
  foo <- spy "foo" <$> parse msg1
  bar <- spy "bar" <$> parse msg2
  baz <- spy "baz" <$> parse msg3
  demonitor ref
  pure unit

-- runAccumCont
--   :: forall m r s a b
--    . (s -> VariantF r (s -> m b) -> m b)
--   -> (s -> a -> m b)
--   -> s
--   -> Run r a
--   -> m b

handle_info :: forall r acc. ProcessDefault r acc => ProcessHandler r acc => Run r Unit -> Effect (Record acc)
handle_info myGenServer = do
  runAccumCont (handle :: Record acc -> VariantF r (Record acc -> Effect (Record acc)) -> Effect (Record acc)) done (def (Proxy :: _ r)) myGenServer
  where
  done acc _ = do
    Console.log "Done!"
    pure acc

--main :: ?w
main =
  handle_info myGenServer
