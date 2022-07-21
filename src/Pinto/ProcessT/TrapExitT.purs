module Pinto.ProcessT.TrapExitT
  ( TrapExitT
  )
  where

import Prelude

import Control.Monad.Identity.Trans (IdentityT, runIdentityT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect.Class (class MonadEffect)
import Erl.Process.Raw (setProcessFlagTrapExit)
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans, initialise, parseForeign, run)
import Pinto.Types (ExitMessage(..), parseTrappedExitFFI)
import Type.Prelude (Proxy(..))

newtype TrapExitT :: forall k. (k -> Type) -> k -> Type
newtype TrapExitT m a = TrapExitT (IdentityT m a)

derive newtype instance Functor m => Functor (TrapExitT m)
derive newtype instance Monad m => Apply (TrapExitT m)
derive newtype instance Monad m => Applicative (TrapExitT m)
derive newtype instance Monad m => Bind (TrapExitT m)
derive newtype instance Monad m => Monad (TrapExitT m)

derive newtype instance MonadEffect m => MonadEffect (TrapExitT m)
derive newtype instance MonadTrans TrapExitT

instance
  (MonadProcessTrans m innerState appMsg innerOutMsg, Monad m) =>
  MonadProcessTrans (TrapExitT m) innerState appMsg (Either ExitMessage innerOutMsg) where
  parseForeign fgn = do
      case parseTrappedExitFFI fgn Exit of
        Just exitMsg -> TrapExitT $ do
          pure $ Left exitMsg
        Nothing -> do
          lift $ Right <$> parseForeign fgn
  -- run (TrapExitT mt) (Tuple _ is) = do
  --     (Tuple (Tuple res newMtState) newIs) <- run is
  --     pure $ Tuple res $ Tuple newMtState newIs
  run (TrapExitT mt) is =
      run (runIdentityT mt) is

  initialise _ = do
    void $ setProcessFlagTrapExit true
    innerState <- initialise (Proxy :: Proxy m)
    pure $ spy "inner"  innerState
