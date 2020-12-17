module Test.ValueServer
  ( ValueServerPid
  , startLink
  , getValue
  , setValue
  , setValueAsync
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Pinto.GenServer (CallResult(..), ReturnResult(..), ServerRunning(..))
import Pinto.GenServer as GS
import Pinto.Types (InstanceRef(..), RegistryName(..), ServerPid, crashIfNotStarted)

type Cont = Void
type Stop = Void
data Msg
  = SetValue Int

type State =
  { value :: Int }



newtype ValueServerPid = ValueServerPid (ServerPid State Msg)
-- instance toRawPid :: ToRawPid ValueServerPid
--   rawPid (ValueServerPid serverPid) = rawPid serverPid


serverName :: RegistryName State Msg
serverName = Local $ atom "valueServer"

startLink :: Effect ValueServerPid
startLink = do
  ValueServerPid
    <$> crashIfNotStarted
    <$> (GS.startLink $ (GS.mkSpec init) { name = Just serverName })
  where
    init =
      let
        state = { value: 0 }
      in
        pure $ Right $ InitOk state

getValue :: Effect Int
getValue =
  GS.call (ByName serverName) impl
  where
    impl _from state@{value}
      = pure $ GS.reply value state

setValue :: Int -> Effect Int
setValue n =
  GS.call (ByName serverName) impl
  where
    impl _from state@{value}
      = pure $ GS.reply value state{value = n}


setValueAsync :: Int -> Effect Unit
setValueAsync n =
  GS.cast (ByName serverName) impl
  where
    impl state@{value}
      = pure $ GS.return state{value = n}
