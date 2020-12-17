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


-- Sequence of types
-- reply cont stop msg [Timeout] state
--------------------------------------------------------------------------------
-- Public types
--------------------------------------------------------------------------------
data CallResult reply cont stop state
  = CallReply reply state
  | CallReplyWithTimeout reply Int state
  | CallReplyHibernate reply state
  | CallReplyContinue reply cont state
  | CallNoReply state
  | CallNoReplyWithTimeout Int state
  | CallNoReplyHibernate state
  | CallNoReplyContinue cont state
  | CallStopReply reply stop state
  | CallStopNoReply stop state

instance mapCallResult :: Functor (CallResult reply cont stop) where
  map f (CallReply reply state) = CallReply reply (f state)
  map f (CallReplyWithTimeout reply timeout state) = CallReplyWithTimeout reply timeout (f state)
  map f (CallReplyHibernate reply state) = CallReplyHibernate reply (f state)
  map f (CallReplyContinue reply cont state) = CallReplyContinue reply cont (f state)
  map f (CallNoReply state) = CallNoReply (f state)
  map f (CallNoReplyWithTimeout timeout state) = CallNoReplyWithTimeout timeout (f state)
  map f (CallNoReplyHibernate state) = CallNoReplyHibernate (f state)
  map f (CallNoReplyContinue cont state) = CallNoReplyContinue cont (f state)
  map f (CallStopReply reply stop state) = CallStopReply reply stop (f state)
  map f (CallStopNoReply stop state) = CallStopNoReply stop (f state)


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

type ResultT result cont stop msg state = ReaderT (Context cont stop msg state) Effect result


-- TODO make order of type variables consistent
type InitFn cont stop msg state = ResultT (InitResult cont state) cont stop msg state
type CallFn reply cont stop msg state = state -> ResultT (CallResult reply cont stop state) cont stop msg state
type CastFn cont stop msg state = state -> ResultT (CastResult state) cont stop msg state
type ContinueFn cont stop msg state = cont -> state -> ResultT (ContinueResult state) cont stop msg state
type InfoFn cont stop msg state = msg -> state -> ResultT (InfoResult state) cont stop msg state

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




type ServerSpec cont stop msg state =
  { name :: Maybe (RegistryName state msg)
  , init :: InitFn cont stop msg state
  , handleInfo :: Maybe (InfoFn cont stop msg state)
  }


--------------------------------------------------------------------------------
-- Internal types
--------------------------------------------------------------------------------
type OuterState cont stop msg state
  = { innerState :: state
    , context :: Context cont stop msg state
    }

mkOuterState :: forall cont stop msg state. Context cont stop msg state -> state -> OuterState cont stop msg state
mkOuterState context innerState = {context, innerState}


newtype Context cont stop msg state
  = Context
    { handleInfo :: Maybe (WrappedInfoFn cont stop msg state)
    }

type WrappedInfoFn cont stop msg state = Fn2 msg (OuterState cont stop msg state) (Effect (InfoResult (OuterState cont stop msg state)))
type WrappedCallFn reply cont stop msg state = Fn1 (OuterState cont stop msg state) (Effect (CallResult reply cont stop (OuterState cont stop msg state)))
type WrappedCastFn cont stop msg state = Fn1 (OuterState cont stop msg state) (Effect (CastResult (OuterState cont stop msg state)))

mkSpec :: forall cont stop msg state. InitFn cont stop msg state -> ServerSpec cont stop msg state
mkSpec initFn =
  { name: Nothing
  , init: initFn
  , handleInfo: Nothing
  }

foreign import callFFI :: forall reply cont stop msg state. InstanceRef state msg -> WrappedCallFn reply cont stop msg state -> Effect reply
call :: forall reply cont stop msg state. InstanceRef state msg -> CallFn reply cont stop msg state -> Effect reply
call instanceRef callFn =
  callFFI instanceRef wrappedCallFn

  where
    wrappedCallFn :: WrappedCallFn reply cont stop msg state
    wrappedCallFn =
      let
        handler state@{ innerState, context: handlerContext } = do
          innerResult <- (runReaderT $ callFn innerState) handlerContext
          pure $ (mkOuterState handlerContext) <$> innerResult
      in
        mkFn1 handler


foreign import castFFI :: forall cont stop msg state. InstanceRef state msg -> WrappedCastFn cont stop msg state -> Effect Unit
cast :: forall cont stop msg state. InstanceRef state msg -> CastFn cont stop msg state -> Effect Unit
cast instanceRef castFn =
  castFFI instanceRef wrappedCastFn

  where
    wrappedCastFn :: WrappedCastFn cont stop msg state
    wrappedCastFn =
      let
        handler state@{ innerState, context: handlerContext } = do
          innerResult <- (runReaderT $ castFn innerState) handlerContext
          pure $ (mkOuterState handlerContext) <$> innerResult
      in
        mkFn1 handler


startLink :: forall cont stop msg state. (ServerSpec cont stop msg state) -> Effect (StartLinkResult state msg)
startLink { name: maybeName, init: initFn, handleInfo: maybeHandleInfo } =
  startLinkFFI maybeName initEffect

  where
    context =
      Context
      { handleInfo: wrapHandleInfo <$> maybeHandleInfo
      }

    initEffect :: Effect (InitResult cont (OuterState cont stop msg state))
    initEffect = do
      innerResult <- (runReaderT initFn) context
      pure $ mapInitResult (mkOuterState context) innerResult

    wrapHandleInfo :: InfoFn cont stop msg state -> WrappedInfoFn cont stop msg state
    wrapHandleInfo handleInfo =
      let
        handler msg state@{ innerState, context: handlerContext } = do
          innerResult <- (runReaderT $ handleInfo msg innerState) handlerContext
          pure $ (mkOuterState handlerContext) <$> innerResult
      in
        mkFn2 handler






foreign import startLinkFFI :: forall cont stop msg state. Maybe (RegistryName state msg) -> Effect (InitResult cont (OuterState cont stop msg state)) -> Effect (StartLinkResult state msg)

self :: forall cont stop msg state. ReaderT (Context cont stop msg state) Effect (ServerPid state msg)
self = Reader.lift selfFFI

foreign import selfFFI :: forall state msg. Effect (ServerPid state msg)
