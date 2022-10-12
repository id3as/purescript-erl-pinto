module Pinto.ProcessT.BusT.MetadataBusT
  ( Bus
  , BusMsg(..)
  , BusRef
  , MetadataBusT
  , MetadataBusInternal
  , busRef
  , create
  , delete
  , raise
  , subscribe
  , unsubscribe
  , updateMetadata
  )
  where

import Prelude

import Control.Monad.State.Trans (StateT, get, gets, modify_, put, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, Tuple3, tuple2, uncurry2, uncurry3)
import Erl.Kernel.Erlang (monotonicTime)
import Erl.Process (class HasSelf, self)
import Erl.Types (MonotonicTime)
import Foreign (Foreign)
import Pinto.ProcessT.Internal.Types (class MonadProcessHandled, class MonadProcessRun, class MonadProcessTrans, initialise, parseForeign, run)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Bus :: Type -> Type -> Type -> Type
newtype Bus name msg metadata = Bus name

instance Show name => Show (Bus name msg metadata) where
  show (Bus name) = "Bus " <> show name

newtype BusRef :: Type -> Type -> Type -> Type
newtype BusRef name msg metadata = BusRef name

derive newtype instance Eq name => Eq (BusRef name msg metadata)
derive newtype instance Ord name => Ord (BusRef name msg metadata)
derive newtype instance Show name => Show (BusRef name msg metadata)

busRef :: forall name msg metadata. name -> BusRef name msg metadata
busRef = BusRef

newtype Generation = Generation (Tuple2 MonotonicTime Int)

derive newtype instance Eq Generation
instance Ord Generation where
  compare = \(Generation tg1) -> tg1 # uncurry2 \t1 g1 ->
    \(Generation tg2) -> tg2 # uncurry2 \t2 g2 ->
      compare t1 t2 <> compare g1 g2

instance Show (Generation) where
  show (Generation gen) = "Generation " <> uncurry2 (const show) gen

data SubscriptionResp metadata = SubscriptionResp Generation metadata

data BusMsgInternal msg metadata
  = DataMsgInternal Generation msg
  | MetadataMsgInternal Generation metadata
  | BusTerminatedInternal Generation

data BusMsg msg metadata
  = DataMsg msg
  | MetadataMsg metadata
  | BusTerminated

derive instance (Eq msg, Eq metadata) => Eq (BusMsg msg metadata)
instance (Show msg, Show metadata) => Show (BusMsg msg metadata) where
  show (DataMsg msg) = "DataMsg " <> show msg
  show (MetadataMsg metadata) = "MetadataMsg " <> show metadata
  show BusTerminated = "BusTerminated"

foreign import data BusNameForeign :: Type
foreign import data BusDataForeign :: Type
foreign import data BusMetadataForeign :: Type

newtype MetadataBusInternal msg = MetadataBusInternal
  (Map BusNameForeign
    { generation :: Maybe Generation
    , monitorRef :: Maybe MetadataBusMonitorRef
    , mapper :: BusMsg BusDataForeign BusMetadataForeign -> msg
    }
  )

newtype MetadataBusT msg m a = MetadataBusT (StateT (MetadataBusInternal msg) m a)

derive newtype instance Functor m => Functor (MetadataBusT msg m)
derive newtype instance Monad m => Apply (MetadataBusT msg m)
derive newtype instance Monad m => Applicative (MetadataBusT msg m)
derive newtype instance Monad m => Bind (MetadataBusT msg m)
derive newtype instance Monad m => Monad (MetadataBusT msg m)

derive newtype instance MonadEffect m => MonadEffect (MetadataBusT msg m)
derive newtype instance MonadTrans (MetadataBusT msg)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

create :: forall name msg metadata. BusRef name msg metadata -> metadata -> Effect (Bus name msg metadata)
create busName metadata = do
  t <- monotonicTime
  createImpl busName (Generation (tuple2 t 0)) metadata

foreign import createImpl :: forall name msg metadata. BusRef name msg metadata -> Generation -> metadata -> Effect (Bus name msg metadata)
foreign import deleteImpl :: forall name msg metadata. Bus name msg metadata -> (Generation -> BusMsgInternal msg metadata) -> Effect Unit

delete :: forall name msg metadata. Bus name msg metadata -> Effect Unit
delete busName = deleteImpl busName BusTerminatedInternal

raise :: forall name msg metadata. Bus name msg metadata -> msg -> Effect Unit
raise = raiseImpl

foreign import raiseImpl :: forall name msg metadata. Bus name msg metadata -> msg -> Effect Unit

updateMetadata :: forall name msg metadata. Bus name msg metadata -> metadata -> Effect Unit
updateMetadata = updateMetadataImpl

foreign import updateMetadataImpl :: forall name msg metadata. Bus name msg metadata -> metadata -> Effect Unit

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------
foreign import data MetadataBusPid :: Type
foreign import data MetadataBusMonitorRef :: Type

foreign import subscribeImpl :: forall name msg metadata. BusRef name msg metadata -> Effect (Maybe (Tuple3 Generation metadata MetadataBusMonitorRef))
foreign import unsubscribeImpl :: forall name msg metadata. Maybe MetadataBusMonitorRef -> BusRef name msg metadata -> Effect Unit
foreign import monitorImpl :: MetadataBusPid -> BusNameForeign -> Effect MetadataBusMonitorRef

subscribe
  :: forall name busMsgIn busMetadataIn msgOut m
   . MonadEffect m
  => BusRef name busMsgIn busMetadataIn
  -> (BusMsg busMsgIn busMetadataIn -> msgOut)
  -> MetadataBusT msgOut m (Maybe busMetadataIn)
subscribe bus mapper =
  MetadataBusT do
    resp <- liftEffect $ subscribeImpl bus
    case resp of
      Nothing -> do
        modify_ \(MetadataBusInternal mm) -> MetadataBusInternal (Map.insert (toBusNameForeign bus) { mapper: toMapperForeign mapper, generation: Nothing, monitorRef: Nothing } mm)
        pure Nothing
      Just genMetadataPidRef -> genMetadataPidRef # uncurry3 \gen metadata ref -> do
        modify_ \(MetadataBusInternal mm) -> MetadataBusInternal (Map.insert (toBusNameForeign bus) { mapper: toMapperForeign mapper, generation: Just gen, monitorRef: Just ref } mm)
        pure $ Just $ metadata

unsubscribe
  :: forall name busMsgIn busMetadata msgOut m
   . MonadEffect m
  => BusRef name busMsgIn busMetadata
  -> MetadataBusT msgOut m Unit
unsubscribe bus =
  MetadataBusT do
    maybeRef <- gets \(MetadataBusInternal mm) -> Map.lookup (toBusNameForeign bus) mm >>= _.monitorRef
    modify_ \(MetadataBusInternal mm) -> MetadataBusInternal (Map.delete (toBusNameForeign bus) mm)
    liftEffect $ unsubscribeImpl maybeRef bus

foreign import parseBusMsg :: Foreign -> Maybe (Either (Tuple3 BusNameForeign (BusMsgInternal BusDataForeign BusMetadataForeign) MetadataBusPid) BusNameForeign)

toBusNameForeign :: forall name msg metadata. BusRef name msg metadata -> BusNameForeign
toBusNameForeign = unsafeCoerce

toMapperForeign :: forall msgIn metadataIn outMsg. (BusMsg msgIn metadataIn -> outMsg) -> (BusMsg BusDataForeign BusMetadataForeign -> outMsg)
toMapperForeign = unsafeCoerce

toBusMsg :: forall msg metadata. Maybe Generation -> BusMsgInternal msg metadata -> Maybe { generation :: Generation, message :: BusMsg msg metadata }
toBusMsg currentGeneration busMsgInternal =
  case busMsgInternal of
    MetadataMsgInternal g metadata ->
      lifecycleGeneration g (MetadataMsg metadata)

    BusTerminatedInternal g ->
      lifecycleGeneration g BusTerminated

    DataMsgInternal g msg ->
      case currentGeneration of
        Just c
          -- We do not expect generation to be incremented for DataMsgInternal
          | g >= c ->
              Just { generation: g, message: DataMsg msg }
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
  MonadProcessTrans m innerMetadata appMsg innerOutMsg =>
  MonadProcessTrans (MetadataBusT msgOut m) (Tuple (MetadataBusInternal msgOut) innerMetadata) appMsg (Either msgOut innerOutMsg) where
  parseForeign fgn = MetadataBusT do
    case parseBusMsg fgn of
      Just (Left busNameMsg) ->
        busNameMsg # uncurry3 \busName busMsgInternal busPid -> do
          MetadataBusInternal mtMetadata <- get
          case Map.lookup busName mtMetadata of
            Nothing -> do
              pure Nothing
            Just { generation, mapper, monitorRef: maybeMonitorRef } -> do
              case toBusMsg generation busMsgInternal of
                Nothing -> do
                  pure Nothing
                Just { generation: newGeneration, message: busMsg } -> do
                  monitorRef <- case maybeMonitorRef of
                    Just monitorRef -> pure monitorRef
                    Nothing -> liftEffect $ monitorImpl busPid busName
                  put $ MetadataBusInternal $ Map.insert busName { generation: Just newGeneration, mapper, monitorRef: Just monitorRef } mtMetadata
                  pure $ Just $ Left $ mapper busMsg
      Just (Right busName) -> do
        MetadataBusInternal mtMetadata <- get
        case Map.lookup busName mtMetadata of
          Nothing -> do
            pure Nothing
          Just { mapper, monitorRef } -> do
            put $ MetadataBusInternal $ Map.delete busName mtMetadata
            liftEffect $ unsubscribeImpl monitorRef (BusRef busName)
            pure $ Just $ Left $ mapper BusTerminated
      Nothing -> do
        (map Right) <$> (lift $ parseForeign fgn)

instance
  MonadProcessRun base m innerMetadata appMsg innerOutMsg =>
  MonadProcessRun base (MetadataBusT msgOut m) (Tuple (MetadataBusInternal msgOut) innerMetadata) appMsg (Either msgOut innerOutMsg) where
  run (MetadataBusT mt) (Tuple mtMetadata is) = do
    (Tuple (Tuple res newMtMetadata) newIs) <- run (runStateT mt mtMetadata) is
    pure $ Tuple res $ Tuple newMtMetadata newIs

  initialise _ = do
    innerMetadata <- initialise (Proxy :: Proxy m)
    pure $ Tuple (MetadataBusInternal Map.empty) innerMetadata

instance MonadProcessHandled m handledMsg => MonadProcessHandled (MetadataBusT msgOut m) handledMsg

instance (HasSelf m msg, Monad m) => HasSelf (MetadataBusT msgOut m) msg where
  self = lift self
