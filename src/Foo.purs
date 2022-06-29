module Foo
where

import Prelude

import Bar (MonitorRef, MonitorMsg, TrapExitMsg)
import Bar as Bar
import Data.Functor.Variant (VariantF)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant, expand, inj)
import Data.Variant as Variant
import Debug (spy, traceM)
import Effect (Effect)
import Effect.Console as Console
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process.Raw (Pid)
import Erl.Process.Raw as Raw
import Foreign (Foreign, unsafeToForeign)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder, build)
import Record.Builder as RecordBuilder
import Run (EFFECT, Run, liftEffect, on, runAccumCont)
import Run as Run
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

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

data MonitorF a
  = Monitor Pid (MonitorRef -> a)
  | Demonitor MonitorRef a
  | MonitorParse a

derive instance Functor MonitorF

type MONITOR r = (monitor :: MonitorF | r)

_monitor = Proxy :: Proxy "monitor"

monitor :: forall r. Pid -> Run (MONITOR + r) MonitorRef
monitor pid = Run.lift _monitor (Monitor pid identity)

demonitor :: forall r. MonitorRef -> Run (MONITOR + r) Unit
demonitor ref = Run.lift _monitor (Demonitor ref unit)

monitorParse :: forall r. Foreign -> Run (MONITOR + r) (Maybe MonitorMsg)
monitorParse fgn = Run.lift _monitor (MonitorParse $ Bar.parseMonitorMsg fgn)

--------------------------------------
-- | TrapExit...
data TrapExitF a
  = TrapExitParse a

derive instance Functor TrapExitF

type TRAPEXIT r = (trapExit :: TrapExitF | r)

_trapExit = Proxy :: Proxy "trapExit"

trapExitParse :: forall r. Foreign -> Run (TRAPEXIT + r) (Maybe TrapExitMsg)
trapExitParse fgn = Run.lift _trapExit (TrapExitParse $ Bar.parseExitMsg fgn)

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

instance ParseType Effect (EFFECT ()) Void where
  parseType _ fgn = pure Nothing

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

else instance
  ( ProcessHandlerRL stackLTail stackTail acc
  , Row.Cons sym Effect stackTail stack
  , IsSymbol sym
  ) =>
  ProcessHandlerRL (RL.Cons sym Effect stackLTail) stack acc where
  handleL _ acc variant = do
    let
      fn :: Effect (Record acc -> Effect (Record acc)) -> Effect (Record acc)
      fn functorInstance = do
        cb' <- functorInstance
        cb' acc

      recurse :: VariantF stackTail (Record acc -> (Effect (Record acc))) -> Effect (Record acc)
      recurse = handleL (Proxy :: _ stackLTail) acc
    on (Proxy :: _ sym) fn recurse variant

-- else instance
--   ( ProcessHandlerRL stackLTail stackTail acc
--   , Row.Cons sym (ProcessM msg) stackTail stack
--   , IsSymbol sym
--   ) =>
--   ProcessHandlerRL (RL.Cons sym (ProcessM msg) stackLTail) stack acc where
--   handleL _ acc variant = do
--     let
--       fn :: (ProcessM msg) (Record acc -> Effect (Record acc)) -> Effect (Record acc)
--       fn (ProcessMParse cb) = cb acc

--       recurse :: VariantF stackTail (Record acc -> (Effect (Record acc))) -> Effect (Record acc)
--       recurse = handleL (Proxy :: _ stackLTail) acc
--     on (Proxy :: _ sym) fn recurse variant

-- else instance
--   ( ProcessHandlerRL stackLTail stackTail acc
--   , Row.Cons sym TrapExitF stackTail stack
--   , IsSymbol sym
--   ) =>
--   ProcessHandlerRL (RL.Cons sym TrapExitF stackLTail) stack acc where
--   handleL _ acc variant = do
--     let
--       fn :: TrapExitF (Record acc -> Effect (Record acc)) -> Effect (Record acc)
--       fn (TrapExitParse cb) = cb acc

--       recurse :: VariantF stackTail (Record acc -> (Effect (Record acc))) -> Effect (Record acc)
--       recurse = handleL (Proxy :: _ stackLTail) acc
--     on (Proxy :: _ sym) fn recurse variant

else instance
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

        functorAcc' <- handleFunctor typeAcc $ map updateAcc functorInstance
        pure $ Record.set (Proxy :: _ sym) functorAcc' acc

      recurse :: VariantF stackTail (Record acc -> (Effect (Record acc))) -> Effect (Record acc)
      recurse = handleL (Proxy :: _ stackLTail) acc
    on (Proxy :: _ sym) fn recurse variant

class FunctorHandler :: (Type -> Type)  -> Type -> Constraint
class FunctorHandler functorType functorAcc | functorType -> functorAcc where
  handleFunctor :: functorAcc -> functorType (functorAcc -> Effect functorAcc) -> Effect functorAcc

instance FunctorHandler Effect Unit where
  handleFunctor acc cb = do
    cb' <- cb
    cb' acc

instance FunctorHandler (ProcessM msg) Unit where
  handleFunctor acc (ProcessMParse cb) = cb acc

instance FunctorHandler TrapExitF Unit where
  handleFunctor acc (TrapExitParse cb) = cb acc

instance
  FunctorHandler MonitorF (Map MonitorRef Unit) where
  handleFunctor map (Monitor pid cb) = do
    ref <- Bar.monitorImpl pid
    cb ref $ Map.insert ref unit map
  handleFunctor map (Demonitor ref cb) = do
    let
      doTheDeMonitor :: MonitorRef -> Effect Unit
      doTheDeMonitor _ = pure unit
    doTheDeMonitor ref
    cb map
  handleFunctor map (MonitorParse cb) = do
    cb map

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

instance FunctorDefault Effect Unit where
  functorDef _ = unit

instance FunctorDefault TrapExitF Unit where
  functorDef _ = unit

instance FunctorDefault MonitorF (Map MonitorRef Unit) where
  functorDef _ = Map.empty

--------------------------------------
-- | Program...
data AppMsg = AppMsg

type MyStack = (EFFECT + MONITOR + TRAPEXIT + PROCESSM AppMsg + ())
myHandleInfo :: forall r. Run MyStack Unit
myHandleInfo = do
  pid <- liftEffect Raw.self
  ref <- monitor pid
  let
    msg1 = unsafeCoerce 1 :: Foreign
    msg2 = unsafeCoerce 2 :: Foreign
    msg3 = unsafeToForeign AppMsg
  foo <- spy "foo" <$> parse msg1
  bar <- spy "bar" <$> parse msg2
  baz <- spy "baz" <$> parse msg3
  demonitor ref
  pure unit


handle_info :: forall r acc. ProcessDefault r acc => ProcessHandler r acc => Run r Unit -> Effect (Record acc)
handle_info myHandleInfo = do
  runAccumCont handle done (def (Proxy :: _ r)) myHandleInfo
  where
  done acc _ = do
    Console.log "Done!"
    pure acc

type AStack = (EFFECT + TRAPEXIT + PROCESSM AppMsg + ())

run :: Int -> Effect _
run count = do
  start <- Bar.milliseconds
  child <- Raw.spawn do
    _ <- runAccumCont handle (done start) (def (Proxy :: _ AStack)) (doA count)
    pure unit
  Raw.send child AppMsg
  where
  done start acc _ = do
    end <- Bar.milliseconds
    traceM {_msg: "Done!", time: end - start}
    pure acc

  doA :: Int -> Run AStack Unit
  doA count = do
    rawMsg <- liftEffect $ Raw.receive
    msg <- parse rawMsg
    me <- liftEffect Raw.self
    liftEffect $ Raw.send me AppMsg
    Variant.match { trapExit: \_ -> pure unit
                  , effect: \_ -> pure unit
                  , z_processM: \_ -> do
                       me <- liftEffect Raw.self
                       liftEffect $ Raw.send me AppMsg
                       pure unit
                  } msg
    case count of
      0 ->
        pure unit
      _ ->
        doA (count - 1)

--main :: ?w
main =
  handle_info myHandleInfo
