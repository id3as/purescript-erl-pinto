module Bar
where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Foreign (Foreign)
import Pinto.Monitor (MonitorRef)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


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
newtype TrapExitT m msg res = TrapExitT (m msg res)

instance MonadEffect (TrapExitT m msg) where
  liftEffect = unsafeCoerce


instance Functor (TrapExitT m msg) where
  map = unsafeCoerce
instance Apply (TrapExitT m msg) where
  apply = unsafeCoerce
instance Applicative (TrapExitT m msg) where
  pure = unsafeCoerce
instance Bind (TrapExitT m msg) where
  bind = unsafeCoerce
instance Monad (TrapExitT m msg)

instance
  FFIParseT (m msg) msg2 =>
  FFIParseT (TrapExitT m msg) (Either TrapExitMsg msg2) where
  psFromFFI :: _ -> Foreign -> Either TrapExitMsg msg2
  psFromFFI _ fgn = do
    case parseExitMsg fgn of
      Just psMsg -> Left psMsg
      Nothing -> Right $ psFromFFI (Proxy ::_ (m msg)) fgn

instance
  RunT (m msg) is =>
  RunT (TrapExitT m msg) is where
  runT (TrapExitT inner) is = do
    runT inner is



class RunT m ms | m -> ms where
  runT :: forall a. m a -> ms -> Effect (Tuple a ms)

class FFIParseT m outMsg | m -> outMsg where
  psFromFFI :: Proxy m  -> Foreign -> outMsg


instance FFIParseT (ProcessM outMsg) outMsg where
  psFromFFI :: Proxy _ -> Foreign -> outMsg
  psFromFFI  = unsafeCoerce

instance RunT (ProcessM outMsg) Unit where
  runT t _ = do
    res <-  unsafeRunProcessM t
    pure $ Tuple res unit

data MonitorMsg = MonitorMsg

type MonitorMap msg = Map MonitorRef (MonitorMsg -> msg)
newtype MonitorT monitorMsg m appMsg res = MonitorT (MonitorMap monitorMsg -> (m appMsg (Tuple res (MonitorMap monitorMsg))))

instance MonadEffect (MonitorT monitorMsg m appMsg) where
  liftEffect = unsafeCoerce

instance Functor (MonitorT monitorMsg m appMsg) where
  map = unsafeCoerce
instance Apply (MonitorT monitorMsg m appMsg) where
  apply = unsafeCoerce
instance Applicative (MonitorT monitorMsg m appMsg) where
  pure = unsafeCoerce
instance Bind (MonitorT monitorMsg m appMsg) where
  bind = unsafeCoerce
instance Monad (MonitorT monitorMsg m appMsg)

instance
  RunT (m appMsg) is =>
  RunT (MonitorT monitorMsg m appMsg) (Tuple (MonitorMap monitorMsg) is) where
  runT (MonitorT mt) (Tuple mtState is) = do
    (Tuple (Tuple res newMtState) newIs) <- runT (mt mtState) is
    pure $ Tuple res $ Tuple newMtState newIs

instance  FFIParseT (m appMsg) msg2 =>
  FFIParseT (MonitorT monitorMsg m appMsg) (Either MonitorMsg msg2) where
  psFromFFI :: _ -> Foreign -> Either MonitorMsg msg2
  psFromFFI _ fgn = do
    case parseMonitorMsg fgn of
      Just monitorMsg -> Left monitorMsg
      Nothing -> Right $ psFromFFI (Proxy ::_ (m appMsg)) fgn



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

y :: MonitorT AppMonitorMsg (TrapExitT ProcessM) AppMsg Unit
y = do
  msg :: Either MonitorMsg (Either TrapExitMsg AppMsg)
   <- receive
  case msg of
    Left MonitorMsg -> pure unit
    Right myApplevelMsg -> pure unit



x :: TrapExitT ProcessM AppMsg Unit
x = do
  msg :: Either TrapExitMsg AppMsg
    <- receive
  case msg of
    Left TrapExitMsg -> pure unit
    Right myApplevelMsg -> pure unit

