module Bar
where

import Prelude

import Control.Monad.Identity.Trans (IdentityT(..), runIdentityT)
import Control.Monad.State (StateT, modify, modify_, runStateT)
import Control.Monad.State.Trans (get, put)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Process (ProcessM, self, unsafeRunProcessM, (!))
import Erl.Process as Process
import Erl.Process.Raw (Pid)
import Erl.Process.Raw as Raw
import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


-- | Reference to a monitor, used to stop the monitor once it is started
foreign import data MonitorRef :: Type

class Default a where
  def :: a


instance Default Unit where
  def = unit

instance (Default a, Default b) => Default (Tuple a b) where
  def = Tuple def def



-- newtype ProcessM (msg :: Type) res
--   = ProcessM (Effect res)
-- derive newtype instance functorProcessM :: Functor (ProcessM res)
-- derive newtype instance applyProcessM :: Apply (ProcessM res)
-- derive newtype instance applicativeProcessM :: Applicative (ProcessM res)
-- derive newtype instance bindProcessM :: Bind (ProcessM res)
-- derive newtype instance monadProcessM :: Monad (ProcessM res)

-- instance MonadEffect (ProcessM a) where
--   liftEffect = ProcessM




rawReceive :: Effect Foreign
rawReceive = Raw.receive

data TrapExitMsg = TrapExitMsg
newtype TrapExitT m a = TrapExitT (IdentityT m a)


derive newtype instance Functor m => Functor (TrapExitT m)
derive newtype instance Monad m => Apply (TrapExitT m)
derive newtype instance Monad m => Applicative (TrapExitT m)
derive newtype instance Monad m => Bind (TrapExitT m)
derive newtype instance Monad m => Monad (TrapExitT m)

derive newtype instance MonadEffect m => MonadEffect (TrapExitT m)
derive newtype instance  MonadTrans TrapExitT

instance
  ( FFIParseT m msg2
  , Monad m
  )  =>
  FFIParseT (TrapExitT m) (Either TrapExitMsg msg2) where
  psFromFFI fgn = do
    case parseExitMsg fgn of
      Just psMsg -> pure $ Left psMsg
      Nothing -> do
        lift $ Right  <$> psFromFFI fgn

instance
  RunT m is =>
  RunT (TrapExitT m) is where
  runT (TrapExitT m) is = do
    runT (runIdentityT m) is



class RunT m ms | m -> ms where
  runT :: forall a. m a -> ms -> Effect (Tuple a ms)

class FFIParseT m outMsg | m ->  outMsg where
  psFromFFI :: Foreign -> m outMsg



receive
  :: forall m outMsg.
     FFIParseT m outMsg =>
     MonadEffect m =>
     m outMsg
receive = do
  msg <- liftEffect $ rawReceive
  psFromFFI msg

foreign import monitorImpl :: Pid -> Effect MonitorRef

monitor ::
  forall monitorMsg m.
  MonadEffect m =>
  Pid -> (MonitorMsg -> monitorMsg) -> MonitorT monitorMsg m MonitorRef
monitor pid mapper = do
    MonitorT do
      ref <- liftEffect $ monitorImpl pid
      modify_ \(MonitorMap mm) -> MonitorMap $ Map.insert ref mapper mm
      pure ref



instance FFIParseT (ProcessM outMsg) outMsg where
  psFromFFI   = pure <<< unsafeCoerce

instance RunT (ProcessM outMsg) Unit where
  runT t _ = do
    res <-  unsafeRunProcessM t
    pure $ Tuple res unit


type MonitorObject
  = Foreign

-- | The 'reason' for the monitor being invoked, if this needs unpacking
-- | then FFI will need to be written
type MonitorInfo
  = Foreign

-- | The type of monitor this message is being sent on behalf
data MonitorType
  = Process
  | Port

data MonitorMsg
  = Down MonitorRef MonitorType MonitorObject MonitorInfo

newtype MonitorMap msg = MonitorMap (Map MonitorRef (MonitorMsg -> msg))
instance Default (MonitorMap msg) where
  def = MonitorMap Map.empty


newtype MonitorT monitorMsg m a = MonitorT (StateT (MonitorMap monitorMsg) m a)

derive newtype instance Functor m => Functor (MonitorT monitorMsg m)
derive newtype instance Monad m => Apply (MonitorT monitorMsg m)
derive newtype instance Monad m => Applicative (MonitorT monitorMsg m)
derive newtype instance Monad m => Bind (MonitorT monitorMsg m)
derive newtype instance Monad m => Monad (MonitorT monitorMsg m)

derive newtype instance MonadEffect m => MonadEffect (MonitorT monitorMsg m)
derive newtype instance MonadTrans (MonitorT monitorMsg)


instance
  RunT m is =>
  RunT (MonitorT monitorMsg m) (Tuple (MonitorMap monitorMsg) is) where
  runT (MonitorT mt) (Tuple mtState is) = do
    (Tuple (Tuple res newMtState) newIs) <- runT (runStateT mt mtState) is
    pure $ Tuple res $ Tuple newMtState newIs

instance
  ( FFIParseT m msg
  , MonadEffect m
  ) =>
  FFIParseT (MonitorT monitorMsg m) (Either monitorMsg msg) where
  psFromFFI fgn = do
    case parseMonitorMsg fgn of
      Just down@(Down ref _ _ _) -> MonitorT $ do
        MonitorMap mtState <- get
        case Map.lookup ref mtState of
          Nothing ->
            unsafeCrashWith "Down from unknown monitor"
          Just mapper -> do
            put $ MonitorMap $ Map.delete ref mtState
            pure $ Left $ mapper down
      Nothing -> do
        lift $ Right <$> psFromFFI fgn




foreign import parseMonitorMsg :: Foreign -> Maybe MonitorMsg
foreign import parseExitMsg :: Foreign -> Maybe TrapExitMsg

-- y ------------------------------------------------------------------------
-- m is TrapExitT ProcessM
-- msg is AppMsg

data AppMsg = AppMsg
data AppMonitorMsg = AppMonitorMsg



-- y----
-- m is MonitorT (TrapExitT ProcessM) AppMsg
-- msg  Either MonitorMsg (Either TrapExitMsg AppMsg)


z :: MonitorT AppMonitorMsg (ProcessM AppMsg) Unit
z = do
  msg <- receive
  case msg of
    Left AppMonitorMsg -> pure unit
    Right myApplevelMsg -> pure unit



y :: MonitorT AppMonitorMsg (TrapExitT (ProcessM AppMsg)) Unit
y = do
  msg <- receive
  case msg of
    Left AppMonitorMsg -> pure unit
    Right myApplevelMsg -> pure unit



x :: TrapExitT (ProcessM AppMsg) Unit
x = do
  msg :: Either TrapExitMsg AppMsg
    <- receive
  case msg of
    Left TrapExitMsg -> pure unit
    Right myApplevelMsg -> pure unit


a :: Int -> TrapExitT (ProcessM AppMsg) Unit
a count = do
  msg :: Either TrapExitMsg AppMsg
    <- receive
  case msg of
    Left TrapExitMsg -> pure unit
    Right myApplevelMsg -> do
      me <- lift self
      liftEffect $ me ! AppMsg
      pure unit
  case count of
    0 -> do
      _ <- pure $ spy "done" count
      pure unit
    _ -> a (count -1)

b :: Int -> ProcessM (Either TrapExitMsg AppMsg) Unit
b count = do
  msg :: Either TrapExitMsg AppMsg
    <- Process.receive
  case msg of
    Left TrapExitMsg -> pure unit
    Right myApplevelMsg -> do
      me <- Process.self
      liftEffect $ Process.send me $ Right AppMsg
      pure unit
  case count of
    0 -> do
      _ <- pure $ spy "done" count
      pure unit
    _ -> b (count -1)

main_a :: Int -> Effect Unit
main_a count = do
  void $ runT (a count) def

main_b :: Int -> Effect Unit
main_b count = do
  unsafeRunProcessM $ b count

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust _ (Just a) = a
unsafeFromJust message Nothing = unsafeCrashWith message
