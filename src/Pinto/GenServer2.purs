-- | Module representing the gen_server in OTP
-- | See also 'gen_server' in the OTP docs (https://erlang.org/doc/man/gen_server.html)


module Pinto.GenServer2
  ( ContinueFn
  , InfoFn
  , InitFn
  , InitResult(..)
  , ServerType
  , TerminateFn
  , TestState
  , startLink3
  )
  where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Erl.ModuleName (NativeModuleName, nativeModuleName)
import Erl.Process (ProcessM)
import Foreign (Foreign)
import Pinto.GenServer (ReturnResult, ServerPid)
import Pinto.GenServer as GS
import Pinto.ModuleNames (pintoGenServer)
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans, initialise, parseForeign, run)
import Pinto.ProcessT.MonitorT (MonitorT, spawnMonitor)
import Pinto.Types (RegistryName, ShutdownReason, StartLinkResult)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)



data TestState = TestState Int
data TestCont = TestCont
data TestStop = TestStop
data TestMsg = TestMsg

data TestMonitorMsg = TestMonitorMsg

type InitFn :: forall k. Type -> Type -> (Type -> k) -> k
type InitFn cont state m                =                            m (InitResult cont state)
type InfoFn cont stop parsedMsg state m = parsedMsg -> state ->      m (ReturnResult cont stop state)
type ContinueFn cont stop state m       = cont -> state ->           m (ReturnResult cont stop state)
type TerminateFn state m                = ShutdownReason -> state -> m  Unit

--type GSMonad = MonitorT TestMonitorMsg (ProcessM TestMsg)
type GSMonad = ProcessM TestMsg





-- | The various return values from an init callback
-- | These roughly map onto the tuples in the OTP documentation
data InitResult cont state
  = InitOk state
  | InitOkTimeout state Int
  | InitOkContinue state cont
  | InitOkHibernate state
  | InitStop Foreign
  | InitIgnore

instance Functor (InitResult cont) where
  map f (InitOk state) = InitOk $ f state
  map f (InitOkTimeout state timeout) = InitOkTimeout (f state) timeout
  map f (InitOkContinue state cont) = InitOkContinue (f state) cont
  map f (InitOkHibernate state) = InitOkHibernate $ f state
  map _ (InitStop term) = InitStop term
  map _ InitIgnore = InitIgnore



fi :: InitFn TestCont TestState GSMonad
fi = fooInit

fooInit :: GSMonad (InitResult TestCont TestState)
fooInit = do
  --_ <- spawnMonitor exitsImmediately $ const TestMonitorMsg
  pure $ InitOk $ TestState 0




--fooHI :: _ -> _ -> GSMonad _ --(ReturnResult TestCont TestStop TestState)
fooHI msg state = do
  case msg of
    --Left TestMonitorMsg ->  pure $ GS.return state
    TestMsg ->  pure $ GS.return state


--x :: Record (AllConfig TestCont TestStop (Either TestMonitorMsg TestMsg) TestState (MonitorT TestMonitorMsg (ProcessM TestMsg)))
-- x =
--   startLink
--      { init
--      , handleInfo: Just fooHI
--      }
--   where
--     init :: InitFn TestCont TestState GSMonad
--     init = do
--       --_ <- spawnMonitor exitsImmediately $ const TestMonitorMsg
--       pure $ InitOk $ TestState 0


-- y =
--   startLink2 (Proxy :: Proxy GSMonad)
--      { init
--      , handleInfo: Just fooHI
--      }
--   where
--     init = do
--       --_ <- spawnMonitor exitsImmediately $ const TestMonitorMsg
--       pure $ InitOk $ TestState 0



z =
  startLink' { init
             , handleContinue
             }
  where
    init :: InitFn _ _ GSMonad
    init = do
      --_ <- spawnMonitor exitsImmediately $ const TestMonitorMsg
      pure $ InitOk $ TestState 0

    handleContinue _ state = pure $ GS.return state

--newtype ServerType :: Type -> Type -> Type -> Type -> Type
newtype ServerType cont stop state m
  = ServerType Void



type OptionalConfig cont stop parsedMsg state m =
  ( name           :: Maybe (RegistryName (ServerType cont stop state m))
  , handleInfo     :: Maybe (InfoFn cont stop parsedMsg state m)
  , handleContinue :: Maybe (ContinueFn cont stop state m)
  , terminate      :: Maybe (TerminateFn state m)
  )

--type X cont stop msg parsedMsg state m = Record (MyOptional cont stop msg parsedMsg state m)

type AllConfig cont stop parsedMsg state m   =
  ( init ::  InitFn cont state m
  | OptionalConfig cont stop parsedMsg state m
  )

type GSConfig cont stop parsedMsg state m =
  { | AllConfig cont stop parsedMsg state m }


data TransState
data TransMsg
data TransMonad
data TransRes

newtype Context cont stop parsedMsg state m
  = Context
    { handleInfo     :: Maybe (InfoFn cont stop parsedMsg state m)
    , handleContinue :: Maybe (ContinueFn cont stop state m)
    , terminate      :: Maybe (TerminateFn state m)
    , mState         :: TransState
    , mParse         :: Foreign -> TransMsg
    , mRun           :: TransMonad -> TransState -> Effect (Tuple TransRes TransState)
    }



type OuterState cont stop parsedMsg state m
  = { innerState :: state
    , context :: Context cont stop parsedMsg state m
    }

foreign import startLinkFFI ::
  forall cont stop appMsg parsedMsg state m.
  Maybe (RegistryName (ServerType cont stop state m)) ->
  NativeModuleName ->
  Effect (InitResult cont (OuterState cont stop parsedMsg state m)) ->
  Effect (StartLinkResult (ServerPid cont stop appMsg state))




defaultOptions :: forall cont stop parsedMsg state m. { | OptionalConfig cont stop parsedMsg state m }
defaultOptions
  = { name : Nothing
    , handleInfo : Nothing
    , handleContinue : Nothing
    , terminate: Nothing
    }

exitsImmediately :: ProcessM Void Unit
exitsImmediately = pure unit


data OptionToMaybe
  = OptionToMaybe

-- instance ConvertOption OptionToMaybe "handleInfo" (Maybe (InfoFn cont stop parsedMsg state m)) (Maybe (InfoFn cont stop parsedMsg state m)) where
--   convertOption _ _ val = val
-- else instance ConvertOption OptionToMaybe "handleInfo" (InfoFn cont stop parsedMsg state m) (Maybe (InfoFn cont stop parsedMsg state m)) where
--   convertOption _ _ val = Just val
instance ConvertOption OptionToMaybe "handleInfo" (Maybe a) (Maybe a) where
  convertOption _ _ val = val
else instance ConvertOption OptionToMaybe "handleInfo" a (Maybe a) where
  convertOption _ _ val = Just val
else instance ConvertOption OptionToMaybe "handleContinue" (Maybe a) (Maybe a) where
  convertOption _ _ val = val
else instance ConvertOption OptionToMaybe "handleContinue" a (Maybe a) where
  convertOption _ _ val = Just val
else instance ConvertOption OptionToMaybe "terminate" (Maybe a) (Maybe a) where
  convertOption _ _ val = val
else instance ConvertOption OptionToMaybe "terminate" a (Maybe a) where
  convertOption _ _ val = Just val
else instance ConvertOption OptionToMaybe sym a a where
  convertOption _ _ val = val

-- a = startLink' { init
--                  --                                         , handleContinue : handleContinue
--   --             , terminate : terminate
--                , handleInfo : handleInfo
--                }
--   where
-- --    init :: InitFn TestCont TestState GSMonad
--     init = do
--       pure $ InitOk $ TestState 0


--     --handleInfo ::  TestMsg -> TestState -> ProcessM TestMsg (ReturnResult TestCont TestStop TestState)
--     --handleInfo ::  InfoFn TestCont TestStop TestMsg TestState GSMonad
--     handleInfo msg state = do
--       case msg of
--         --Left TestMonitorMsg ->  pure $ GS.return state
--         TestMsg ->  pure $ GS.return state


--     -- handleContinue :: ContinueFn TestCont TestStop TestState GSMonad
--     handleContinue _ state = pure $ GS.return state

-- --    terminate :: TerminateFn TestState GSMonad
--     terminate _ _ = do
--       pure unit



-- b = startLink { init
--               , handleInfo : Nothing
--               , handleContinue : Nothing
--               , terminate: Nothing --Just terminate
--               }

--   where
--     init :: InitFn TestCont TestState GSMonad
--     init = do
--       pure $ InitOk $ TestState 0


--     --handleInfo ::  TestMsg -> TestState -> ProcessM TestMsg (ReturnResult TestCont TestStop TestState)
--     --handleInfo ::  InfoFn TestCont TestStop TestMsg TestState GSMonad
--     handleInfo msg state = do
--       case msg of
--         --Left TestMonitorMsg ->  pure $ GS.return state
--         TestMsg ->  pure $ GS.return state


--     -- handleContinue :: ContinueFn TestCont TestStop TestState GSMonad
--     handleContinue _ state = pure $ GS.return state

--     terminate :: TerminateFn TestState GSMonad
--     terminate _ _ = do
--       pure unit


startLink'
  :: forall providedConfig cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => ConvertOptionsWithDefaults OptionToMaybe { | OptionalConfig cont stop parsedMsg state m} { | providedConfig } { | AllConfig cont stop parsedMsg state m}
  => { | providedConfig } -> String
startLink' providedConfig =
  let
    config = convertOptionsWithDefaults OptionToMaybe defaultOptions providedConfig
    _ = spy "config" config
  in
    startLink config
  
startLink
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => { | AllConfig cont stop parsedMsg state m } -> String
startLink config = "hello"

startLink2
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => Proxy m -> { | AllConfig cont stop parsedMsg state m } -> String
startLink2 _ = startLink


-- | Given a specification, starts a GenServer
-- |
-- | Standard usage:
-- |
-- | ```purescript
-- | GenServer.startLink $ GenServer.defaultSpec init
-- |   where
-- |   init :: InitFn Unit Unit Unit {}
-- |   init = pure $ InitOk {}
-- | ```

startLink3
  :: forall cont stop appMsg parsedMsg state m mState
   . MonadProcessTrans m mState appMsg parsedMsg
  => GSConfig cont stop parsedMsg state m
  -> Effect (StartLinkResult (ServerPid cont stop appMsg state))
startLink3 { name: maybeName, init: initFn, handleInfo, handleContinue, terminate: terminate' }
  = startLinkFFI maybeName (nativeModuleName pintoGenServer) initEffect
  where
  initEffect :: Effect (InitResult cont (OuterState cont stop parsedMsg state m))
  initEffect = do
    initialMState <- initialise (Proxy :: Proxy m)
    Tuple innerResult newMState  <- run initFn initialMState
    pure $ { context: initialContext newMState, innerState: _ } <$> innerResult

  initialContext :: mState -> Context cont stop parsedMsg state m
  initialContext mState = Context { handleInfo : handleInfo
                                  , handleContinue : handleContinue
                                  , terminate: terminate'
                                  , mState : unsafeCoerce mState
                                  , mParse : unsafeCoerce (parseForeign :: (Foreign -> m parsedMsg))
                                  , mRun : unsafeCoerce (run :: forall a. m a -> mState -> Effect (Tuple a mState))
                                  }
