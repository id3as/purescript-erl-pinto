module Pinto.ProcessT.BusT.StateBusT where

import Prelude

import Control.Monad.State.Trans (StateT, get, modify_, put, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, fst, snd)
import Erl.Process (class HasSelf, self)
import Foreign (Foreign)
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans, initialise, parseForeign, run)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class UpdateState state msg where
  updateState :: msg -> state -> state

newtype Bus :: Type -> Type -> Type -> Type
newtype Bus name msg state = Bus name

newtype BusRef :: Type -> Type -> Type -> Type
newtype BusRef name msg state = BusRef name

busRef :: forall name msg state. name -> BusRef name msg state
busRef = BusRef

newtype Generation = Generation Int

derive newtype instance Eq Generation
derive newtype instance Ord Generation
instance Show (Generation) where
  show (Generation gen) = "Generation " <> show gen

data BusMsgInternal msg state
  = DataMsg Generation msg
  | InitialStateMsg Generation state
  | BusTerminatedInternal Generation

data BusMsg msg state
   = Msg msg
   | State state
   | BusTerminated

foreign import data BusNameForeign :: Type
foreign import data BusMsgForeign :: Type
foreign import data BusStateForeign :: Type

type StateBusInternal msg =
  Map BusNameForeign { generation :: Maybe Generation, mapper :: BusMsg BusMsgForeign BusStateForeign -> msg }

newtype StateBusT msg m a = StateBusT (StateT (StateBusInternal msg) m a)

derive newtype instance Functor m => Functor (StateBusT msg m)
derive newtype instance Monad m => Apply (StateBusT msg m)
derive newtype instance Monad m => Applicative (StateBusT msg m)
derive newtype instance Monad m => Bind (StateBusT msg m)
derive newtype instance Monad m => Monad (StateBusT msg m)

derive newtype instance MonadEffect m => MonadEffect (StateBusT msg m)
derive newtype instance MonadTrans (StateBusT msg)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

foreign import create :: forall name msg state. BusRef name msg state -> state -> Effect (Bus name msg state)
foreign import deleteImpl :: forall name msg state. Bus name msg state -> (Generation -> BusMsgInternal msg state) -> Effect Unit

delete :: forall name msg state. Bus name msg state -> Effect Unit
delete busName = deleteImpl busName BusTerminatedInternal

{-
derive instance (Eq msg, Eq state) => Eq (BusMsg msg state)
instance (Show msg, Show state) => Show (BusMsg msg state) where
  show (DataMsg versionedMsg) = "DataMsg " <> show versionedMsg
  show (InitialStateMsg state) = "InitialStateMsg " <> show state
  show BusTerminated = "BusTerminated"
-}

raise :: forall name msg state. UpdateState state msg => Bus name msg state -> msg -> Effect Unit
raise = raiseImpl updateState

foreign import raiseImpl :: forall name msg state. (msg -> state -> state) -> Bus name msg state -> msg -> Effect Unit

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------
foreign import subscribeImpl :: forall name msg state. BusRef name msg state -> Effect Unit
foreign import unsubscribeImpl :: forall name msg state. BusRef name msg state -> Effect Unit


subscribe
  :: forall name busMsgIn busStateIn msgOut m
   . MonadEffect m
  => BusRef name busMsgIn busStateIn
  -> (BusMsg busMsgIn busStateIn -> msgOut)
  -> StateBusT msgOut m Unit
subscribe bus mapper =
  StateBusT do
    modify_ \mm -> Map.insert (toBusNameForeign bus) { generation: Nothing, mapper: toMapperForeign mapper } mm
    liftEffect $ subscribeImpl bus

unsubscribe
  :: forall name busMsgIn busState msgOut m
   . MonadEffect m
  => BusRef name busMsgIn busState
  -> StateBusT msgOut m Unit
unsubscribe bus =
  StateBusT do
    modify_ \mm -> Map.delete (toBusNameForeign bus) mm
    liftEffect $ unsubscribeImpl bus


foreign import parseBusMsg :: Foreign -> Maybe (Tuple2 BusNameForeign (BusMsgInternal BusMsgForeign BusStateForeign))

toBusNameForeign :: forall name msg state. BusRef name msg state -> BusNameForeign
toBusNameForeign = unsafeCoerce

toMapperForeign :: forall msgIn stateIn outMsg. (BusMsg msgIn stateIn -> outMsg) -> (BusMsg BusMsgForeign BusStateForeign -> outMsg)
toMapperForeign = unsafeCoerce

toBusMsg :: forall msg state. Maybe Generation -> BusMsgInternal msg state -> Maybe { generation :: Generation, message :: BusMsg msg state }
toBusMsg currentGeneration busMsgInternal =
  case busMsgInternal of
    InitialStateMsg g state ->
      lifecycleGeneration g (State state)

    BusTerminatedInternal g ->
      lifecycleGeneration g BusTerminated

    DataMsg g msg ->
      case currentGeneration of
        Just c
          | g > c ->
            Just { generation: g, message: Msg msg }
          | otherwise ->
            Nothing
        Nothing ->
          Nothing
  where
  lifecycleGeneration g message =
    case currentGeneration of
      Nothing ->
        Just { generation: g, message }
      Just c
        | g > c ->
          Just { generation: g, message }
        | otherwise ->
          Nothing

instance
  ( MonadProcessTrans m innerState appMsg innerOutMsg
  , Monad m
  ) =>
  MonadProcessTrans (StateBusT msgOut m) (Tuple (StateBusInternal msgOut) innerState) appMsg (Either msgOut innerOutMsg) where
  parseForeign fgn = StateBusT do
    case parseBusMsg fgn of
      Just busNameMsg -> do
        let busName = fst busNameMsg
        let busMsgInternal = snd busNameMsg
        mtState <- get
        case Map.lookup busName mtState of
          Nothing ->
            pure Nothing
          Just { generation, mapper } -> do
            case toBusMsg generation busMsgInternal of
              Nothing -> pure Nothing
              Just { generation: newGeneration, message: busMsg } -> do
                put $ Map.insert busName { generation: Just newGeneration, mapper} mtState
                pure $ Just $ Left $ mapper busMsg
      Nothing -> do
        (map Right) <$> (lift $ parseForeign fgn)

  run (StateBusT mt) (Tuple mtState is) = do
    (Tuple (Tuple res newMtState) newIs) <- run (runStateT mt mtState) is
    pure $ Tuple res $ Tuple newMtState newIs

  initialise _ = do
    innerState <- initialise (Proxy :: Proxy m)
    pure $ Tuple Map.empty innerState

instance (HasSelf m msg, Monad m) => HasSelf (StateBusT msgOut m) msg where
  self = lift self

--                        Register

--      S0
-- M1   S1
-- M2   S2
-- M3   S3

-- Sender: On create raises the State message for anyone that is already registered

-- Register after creation

--      S0
-- M1   S1
--                              Register - join the bus              M2
--                                         lookup the current state  S2
-- M2   S2
-- M3   S3

-- If we get a message with a smaller monotonic time - ignore it
-- If we get a delta message older than or as old as our state - ignore it
-- If we get a delta message and have no state, then crash
-- If we get a state message when we already have state, ignore it

-- If there is no state when we regist, then all good - we'll get sent state when the bus initialises

-- S0 -> S0

-- Recipient
-- 1) register for message
-- 2) read current state

-- Sender - create - make a new monotonic time
-- 1) Write current state
-- 2) Raise S0

-- Sender - raise
-- 1) Update current state and write it (Sn)
-- 2) Raise Mn