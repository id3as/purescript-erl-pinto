module Bar
where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process.Raw (Pid)
import Foreign (Foreign)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


-- | Reference to a monitor, used to stop the monitor once it is started
foreign import data MonitorRef :: Type


newtype ProcessM (msg :: Type) res
  = ProcessM (Effect res)
derive newtype instance functorProcessM :: Functor (ProcessM res)
derive newtype instance applyProcessM :: Apply (ProcessM res)
derive newtype instance applicativeProcessM :: Applicative (ProcessM res)
derive newtype instance bindProcessM :: Bind (ProcessM res)
derive newtype instance monadProcessM :: Monad (ProcessM res)

instance MonadEffect (ProcessM a) where
  liftEffect = ProcessM


unsafeRunProcessM :: forall a b. ProcessM a b -> Effect b
unsafeRunProcessM (ProcessM b) = b


rawReceive :: Effect Foreign
rawReceive = unsafeCoerce 1

data TrapExitMsg = TrapExitMsg
--newtype TrapExitT :: (Type -> Type -> Type) -> Type -> Type -> Type
newtype TrapExitT m res = TrapExitT (m res)

instance MonadEffect (TrapExitT m) where
  liftEffect = unsafeCoerce


instance Functor (TrapExitT m) where
  map = unsafeCoerce
instance Apply (TrapExitT m) where
  apply = unsafeCoerce
instance Applicative (TrapExitT m) where
  pure = unsafeCoerce
instance Bind (TrapExitT m) where
  bind = unsafeCoerce
instance Monad (TrapExitT m)

instance
  FFIParseT m msg2 =>
  FFIParseT (TrapExitT m) (Either TrapExitMsg msg2) where
  psFromFFI :: _ -> Foreign -> Either TrapExitMsg msg2
  psFromFFI _ fgn = do
    case parseExitMsg fgn of
      Just psMsg -> Left psMsg
      Nothing -> Right $ psFromFFI (Proxy ::_ m) fgn

instance
  RunT m is =>
  RunT (TrapExitT m) is where
  runT (TrapExitT inner) is = do
    runT inner is



class RunT m ms | m -> ms where
  runT :: forall a. m a -> ms -> Effect (Tuple a ms)

class FFIParseT :: (Type -> Type) -> Type -> Constraint
class FFIParseT m outMsg | m -> outMsg where
  psFromFFI :: Proxy m  -> Foreign -> outMsg

foreign import monitorImpl :: Pid -> Effect MonitorRef





--              MonitorMap monitorMsg -> (m (Tuple res (MonitorMap monitorMsg)))
monitor
  :: forall monitorMsg m.
  MonadEffect m =>
  Pid -> (MonitorRef -> monitorMsg) -> MonitorT monitorMsg m MonitorRef
monitor pid mapper =
  MonitorT (\mMap -> do
               ref <- liftEffect $ monitorImpl pid
               pure $ Tuple ref $ Map.insert ref mapper mMap
               --unsafeCoerce 1

             )

instance FFIParseT (ProcessM outMsg) outMsg where
  psFromFFI :: Proxy _ -> Foreign -> outMsg
  psFromFFI  = unsafeCoerce

instance RunT (ProcessM outMsg) Unit where
  runT t _ = do
    res <-  unsafeRunProcessM t
    pure $ Tuple res unit

data MonitorMsg = MonitorMsg

type MonitorMap msg = Map MonitorRef (MonitorRef -> msg)


--newtype MonitorT :: Type -> (Type -> Type -> Type) -> Type -> Type -> Type

newtype MonitorT monitorMsg m res = MonitorT (MonitorMap monitorMsg -> m (Tuple res (MonitorMap monitorMsg)))

-- runMonitorT ::
--   forall monitorMsg m res.
--   MonitorT (MonitorMap monitorMsg -> (m (Tuple res (MonitorMap monitorMsg)))) -> MonitorMap monitorMsg -> m (Tuple res (MonitorMap monitorMsg))
--runMonitorT:: forall monitorMsg m a. MonitorT MonitorMsg m a -> MonitorMap monitorMsg -> m (Tuple a (MonitorMap monitorMsg))
--runMonitorT :: forall monitorMsg m res. MonitorT (MonitorMap monitorMsg -> m (Tuple res (MonitorMap monitorMsg))) ->  MonitorMap monitorMsg -> m (Tuple res (MonitorMap monitorMsg))
--runMonitorT :: forall t415 t416 t417 t418. MonitorT t415 t416 t417 t418 -> Map MonitorRef (MonitorRef -> t415) -> t416 t417 (Tuple t418 (Map MonitorRef (MonitorRef -> t415)))
--runMonitorT (MonitorT mt) = mt

instance Monad m => Functor (MonitorT monitorMsg m) where
  map f (MonitorT mt) = MonitorT \monitorMap -> do
    (Tuple res newMontorMap) <- mt monitorMap
    pure $ Tuple (f res) newMontorMap

instance Monad m => Apply (MonitorT monitorMsg m) where
  apply = unsafeCoerce
instance Monad m => Applicative (MonitorT monitorMsg m) where
  pure = unsafeCoerce
instance Monad m => Bind (MonitorT monitorMsg m) where
  bind = unsafeCoerce
instance Monad m => Monad (MonitorT monitorMsg m)

instance Monad m => MonadEffect (MonitorT monitorMsg m) where
  liftEffect = unsafeCoerce



instance
  RunT m is =>
  RunT (MonitorT monitorMsg m) (Tuple (MonitorMap monitorMsg) is) where
  runT (MonitorT mt) (Tuple mtState is) = do
    (Tuple (Tuple res newMtState) newIs) <- runT (mt mtState) is
    pure $ Tuple res $ Tuple newMtState newIs

instance  FFIParseT m msg2 =>
  FFIParseT (MonitorT monitorMsg m) (Either MonitorMsg msg2) where
  psFromFFI :: _ -> Foreign -> Either MonitorMsg msg2
  psFromFFI _ fgn = do
    case parseMonitorMsg fgn of
      Just monitorMsg -> Left monitorMsg
      Nothing -> Right $ psFromFFI (Proxy ::_ m) fgn



foreign import parseMonitorMsg :: Foreign -> Maybe MonitorMsg
foreign import parseExitMsg :: Foreign -> Maybe TrapExitMsg

-- y ------------------------------------------------------------------------
-- m is TrapExitT ProcessM
-- msg is AppMsg

data AppMsg = AppMsg
data AppMonitorMsg = AppMonitorMsg


receive :: forall m outMsg. MonadEffect m => FFIParseT m outMsg => m outMsg
receive =
  psFromFFI (Proxy :: _ m) <$> liftEffect rawReceive

-- y----
-- m is MonitorT (TrapExitT ProcessM) AppMsg
-- msg  Either MonitorMsg (Either TrapExitMsg AppMsg)


z :: MonitorT AppMonitorMsg (ProcessM AppMsg) Unit
z = do
  msg :: Either MonitorMsg  AppMsg
   <- receive
  case msg of
    Left MonitorMsg -> pure unit
    Right myApplevelMsg -> pure unit



y :: MonitorT AppMonitorMsg (TrapExitT (ProcessM AppMsg)) Unit
y = do
  msg :: Either MonitorMsg (Either TrapExitMsg AppMsg)
   <- receive
  case msg of
    Left MonitorMsg -> pure unit
    Right myApplevelMsg -> pure unit



x :: TrapExitT (ProcessM AppMsg) Unit
x = do
  msg :: Either TrapExitMsg AppMsg
    <- receive
  case msg of
    Left TrapExitMsg -> pure unit
    Right myApplevelMsg -> pure unit
