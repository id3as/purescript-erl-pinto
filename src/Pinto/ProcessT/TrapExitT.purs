module Pinto.ProcessT.TrapExitT
  -- ( TrapExitMsg(..)
  -- , TrapExitT
  -- )
  where

x = 1
{-
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
import Erl.Process (Process)
import Erl.Process.Raw (class HasPid, getPid)
import Erl.Process.Raw as Raw
import Foreign (Foreign)
import Partial.Unsafe (unsafeCrashWith)
import Pinto.GenServer (ExitMessage)
import Pinto.ProcessT (spawn, spawnLink)
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans, initialise, parseForeign, run)
import Type.Prelude (Proxy(..))

newtype TrapExitT exitMsg m a = TrapExitT (StateT (ExitMessage -> exitMsg) m a)

derive newtype instance Functor m => Functor (TrapExitT exitMsg m)
derive newtype instance Monad m => Apply (TrapExitT exitMsg m)
derive newtype instance Monad m => Applicative (TrapExitT exitMsg m)
derive newtype instance Monad m => Bind (TrapExitT exitMsg m)
derive newtype instance Monad m => Monad (TrapExitT exitMsg m)

derive newtype instance MonadEffect m => MonadEffect (TrapExitT exitMsg m)
derive newtype instance MonadTrans (TrapExitT exitMsg)

foreign import parseTrapExitMsg :: Foreign -> Maybe TrapExitMsg

instance
  (MonadProcessTrans m innerState appMsg innerOutMsg, Monad m) =>
  MonadProcessTrans (TrapExitT exitMsg m) (Tuple (ExitMessage -> exitMsg) innerState) appMsg (Either exitMsg innerOutMsg) where
  parseForeign fgn = do
      case parseMonitorMsg fgn of
        Just down@(Down ref _ _ _) -> TrapExitT $ do
          mtState <- get
          case Map.lookup ref mtState of
            Nothing ->
              unsafeCrashWith "Down from unknown monitor"
            Just mapper -> do
              put $ Map.delete ref mtState
              pure $ Left $ mapper down
        Nothing -> do
          lift $ Right <$> parseForeign fgn
  run (TrapExitT mt) (Tuple mtState is) = do
      (Tuple (Tuple res newMtState) newIs) <- run (runStateT mt mtState) is
      pure $ Tuple res $ Tuple newMtState newIs
  initialise _ = do
    setProcessFlagTrapExit
    innerState <- initialise (Proxy :: Proxy m)
    pure $ Tuple Map.empty innerState

-}
