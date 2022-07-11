module Pinto.ProcessT.MonitorT
  ( MonitorT
  , MonitorInfo
  , MonitorMsg(..)
  , MonitorObject
  , MonitorRef
  , MonitorType
  , monitor
  , MonitorMap -- deleteME
  ) where

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
import Erl.Process.Raw as Raw
import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.ProcessT.Internal.Types (class FFIParseT, class InitialState, class RunT, psFromFFI, runT)

newtype MonitorT monitorMsg m a = MonitorT (StateT (MonitorMap monitorMsg) m a)

derive newtype instance Functor m => Functor (MonitorT monitorMsg m)
derive newtype instance Monad m => Apply (MonitorT monitorMsg m)
derive newtype instance Monad m => Applicative (MonitorT monitorMsg m)
derive newtype instance Monad m => Bind (MonitorT monitorMsg m)
derive newtype instance Monad m => Monad (MonitorT monitorMsg m)

derive newtype instance MonadEffect m => MonadEffect (MonitorT monitorMsg m)
derive newtype instance MonadTrans (MonitorT monitorMsg)

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


-- | Reference to a monitor, used to stop the monitor once it is started
foreign import data MonitorRef :: Type

newtype MonitorMap msg = MonitorMap (Map MonitorRef (MonitorMsg -> msg))

instance (RunT m is) =>
  RunT (MonitorT monitorMsg m) (Tuple (MonitorMap monitorMsg) is) where
  runT  = x
   where
    --x :: ?t
    x (MonitorT mt) (Tuple mtState is) = do
      (Tuple (Tuple res newMtState) newIs) <- runT (runStateT mt mtState) is
      pure $ Tuple res $ Tuple newMtState newIs


foreign import monitorImpl :: Raw.Pid -> Effect MonitorRef
foreign import parseMonitorMsg :: Foreign -> Maybe MonitorMsg

instance InitialState (MonitorMap monitorMsg) where
  initialState = pure $ MonitorMap Map.empty

instance
  ( FFIParseT m msg
  , MonadEffect m
  ) =>
  FFIParseT (MonitorT monitorMsg m) (Either monitorMsg msg) where
  psFromFFI = x
   where
     -- x :: ?t
     x fgn = do
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



monitor ::
  forall monitorMsg m.
  MonadEffect m =>
  Raw.Pid -> (MonitorMsg -> monitorMsg) -> MonitorT monitorMsg m MonitorRef
monitor pid mapper = do
    MonitorT do
      ref <- liftEffect $ monitorImpl pid
      modify_ \(MonitorMap mm) -> MonitorMap $ Map.insert ref mapper mm
      pure ref

