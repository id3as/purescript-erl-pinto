-- | Module representing the gen_server in OTP
-- | See also 'gen_server' in the OTP docs (https://erlang.org/doc/man/gen_server.html)
module Pinto.GenServer2
  ( ContinueFn
  , InfoFn
  , InitFn
  , TerminateFn
  , TestState
  , startLink
  )
  where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import Erl.Process (ProcessM)
import Pinto.GenServer (InitResult(..), ReturnResult, terminate)
import Pinto.GenServer as GS
import Pinto.ProcessT.Internal.Types (class MonadProcessTrans)
import Pinto.ProcessT.MonitorT (MonitorT, spawnMonitor)
import Pinto.Types (ShutdownReason)
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

type OptionalConfig cont stop parsedMsg state m =
  ( handleInfo     :: Maybe (InfoFn cont stop parsedMsg state m)
  , handleContinue :: Maybe (ContinueFn cont stop state m)
  , terminate      :: Maybe (TerminateFn state m)
  )

--type X cont stop msg parsedMsg state m = Record (MyOptional cont stop msg parsedMsg state m)

type AllConfig cont stop parsedMsg state m   =
  ( init ::  InitFn cont state m
  | OptionalConfig cont stop parsedMsg state m
  )

defaultOptions :: forall cont stop parsedMsg state m. { | OptionalConfig cont stop parsedMsg state m }
defaultOptions
  = { handleInfo : Nothing
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
