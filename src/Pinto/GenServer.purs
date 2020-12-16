module Pinto.GenServer
  ( InitFn
  , InitResult(..)
  , CallFn
  , CallResult(..)
  , CastFn
  , CastResult(..)
  , InfoFn
  , InfoResult(..)
  , ResultT
  , ServerRunning(..)
  , ServerNotRunning(..)
  , Context
  , mkSpec
  , startLink
  , call
  , cast
  , self
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader as Reader
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn1, mkFn1, mkFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign (Foreign)
import Pinto.Types (InstanceRef, RegistryName, ServerPid, StartLinkResult)



--------------------------------------------------------------------------------
-- Public types
--------------------------------------------------------------------------------
data CallResult reply state
  = CallReply reply state
  -- | CallReplyWithTimeout reply state Int
  -- | CallReplyHibernate reply state
  -- | CallReplyContinue reply state cont
  -- | CallNoReply state
  -- | CallNoReplyWithTimeout state Int
  -- | CallNoReplyHibernate state
  -- | CallNoReplyContinue state cont
  -- | CallStopReply stop reply state
  -- | CallStopNoReply stop state

instance mapCallResult :: Functor (CallResult reply) where
  map f (CallReply reply state) = (CallReply reply (f state))

data CastResult state
  = NoReply state
  -- | NoReplyTimeout state Int
  -- | NoReplyHibernate state
  -- | NoReplyContinue state cont
  -- | Stop stop state

instance mapCastResult :: Functor CastResult where
  map f (NoReply state) = NoReply (f state)


type InfoResult state = CastResult state
type ContinueResult state = CastResult state

type ResultT result state msg = ReaderT (Context state msg) Effect result

type InitFn state cont msg = ResultT (InitResult cont state) state msg
type CallFn reply state msg = state -> ResultT (CallResult reply state) state msg
type CastFn state msg = state -> ResultT (CastResult state) state msg
type ContinueFn cont state msg = cont -> state -> ResultT (ContinueResult state) state msg
type InfoFn state msg = msg -> state -> ResultT (InfoResult state) state msg

-- -- | Type of the callback invoked during a gen_server:handle_cast
-- type Cast state msg = ResultT (CastResult state) state msg


data ServerRunning cont state
  = InitOk state
  | InitOkTimeout state Int
  | InitOkContinue state cont
  | InitOkHibernate state

data ServerNotRunning
  = InitStop Foreign
  | InitIgnore

type InitResult cont state = Either ServerNotRunning (ServerRunning cont state )

-- Can't do a functor instance over a type synonym, so just have a function instead
mapInitResult :: forall state state' cont. (state -> state') -> InitResult cont state -> InitResult cont state'
mapInitResult f (Right (InitOk state)) = Right (InitOk $ f state)
mapInitResult f (Right (InitOkTimeout state timeout)) = Right (InitOkTimeout (f state) timeout)
mapInitResult f (Right (InitOkContinue state cont)) = Right (InitOkContinue (f state) cont)
mapInitResult f (Right (InitOkHibernate state)) = Right (InitOkHibernate $ f state)
mapInitResult _ (Left (InitStop term)) = Left (InitStop term)
mapInitResult _ (Left InitIgnore) = Left InitIgnore




type ServerSpec state cont msg =
  { name :: Maybe (RegistryName state msg)
  , init :: InitFn state cont msg
  , handleInfo :: Maybe (InfoFn state msg)
  }


--------------------------------------------------------------------------------
-- Internal types
--------------------------------------------------------------------------------
type OuterState state msg
  = { innerState :: state
    , context :: Context state msg
    }

mkOuterState :: forall state msg. Context state msg -> state -> OuterState state msg
mkOuterState context innerState = { innerState, context }


newtype Context state msg
  = Context
    { handleInfo :: Maybe (WrappedInfoFn state msg)
    }

type WrappedInfoFn state msg = Fn2 msg (OuterState state msg) (Effect (InfoResult (OuterState state msg)))
type WrappedCallFn reply state msg = Fn1 (OuterState state msg) (Effect (CallResult reply (OuterState state msg)))
type WrappedCastFn state msg = Fn1 (OuterState state msg) (Effect (CastResult (OuterState state msg)))

mkSpec :: forall state cont msg. InitFn state cont msg -> ServerSpec state cont msg
mkSpec initFn =
  { name: Nothing
  , init: initFn
  , handleInfo: Nothing
  }

foreign import callFFI :: forall reply state msg . InstanceRef state msg -> WrappedCallFn reply state msg -> Effect reply
call :: forall reply state msg . InstanceRef state msg -> CallFn reply state msg -> Effect reply
call instanceRef callFn =
  callFFI instanceRef wrappedCallFn

  where
    wrappedCallFn :: WrappedCallFn reply state msg
    wrappedCallFn =
      let
        handler state@{ innerState, context: handlerContext } = do
          innerResult <- (runReaderT $ callFn innerState) handlerContext
          pure $ (mkOuterState handlerContext) <$> innerResult
      in
        mkFn1 handler


foreign import castFFI :: forall state msg . InstanceRef state msg -> WrappedCastFn state msg -> Effect Unit
cast :: forall state msg . InstanceRef state msg -> CastFn state msg -> Effect Unit
cast instanceRef castFn =
  castFFI instanceRef wrappedCastFn

  where
    wrappedCastFn :: WrappedCastFn state msg
    wrappedCastFn =
      let
        handler state@{ innerState, context: handlerContext } = do
          innerResult <- (runReaderT $ castFn innerState) handlerContext
          pure $ (mkOuterState handlerContext) <$> innerResult
      in
        mkFn1 handler


startLink :: forall state cont msg. (ServerSpec state cont msg) -> Effect (StartLinkResult state msg)
startLink { name: maybeName, init: initFn, handleInfo: maybeHandleInfo } =
  startLinkFFI maybeName initEffect

  where
    context =
      Context
      { handleInfo: wrapHandleInfo <$> maybeHandleInfo
      }

    initEffect :: Effect (InitResult cont (OuterState state msg))
    initEffect = do
      innerResult <- (runReaderT initFn) context
      pure $ mapInitResult (mkOuterState context) innerResult

    wrapHandleInfo :: InfoFn state msg -> WrappedInfoFn state msg
    wrapHandleInfo handleInfo =
      let
        handler msg state@{ innerState, context: handlerContext } = do
          innerResult <- (runReaderT $ handleInfo msg innerState) handlerContext
          pure $ (mkOuterState handlerContext) <$> innerResult
      in
        mkFn2 handler






foreign import startLinkFFI :: forall state cont msg. Maybe (RegistryName state msg) -> Effect (InitResult cont (OuterState state msg)) -> Effect (StartLinkResult state msg)

self :: forall state msg. ReaderT (Context state msg) Effect (ServerPid state msg)
self = Reader.lift selfFFI

foreign import selfFFI :: forall state msg. Effect (ServerPid state msg)
