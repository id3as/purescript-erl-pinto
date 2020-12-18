module Test.ValueServer
  ( startLink
  , getValue
  , setValue
  , setValueAsync
  , Msg
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Pinto.GenServer (ServerRunning(..), ServerType)
import Pinto.GenServer as GS
import Pinto.Types (InstanceRef(..), RegistryName(..), ServerPid, crashIfNotStarted)

type Cont = Void
type Stop = Void
data Msg
  = SetValue Int

type State = { value :: Int }


type ValueServerType = ServerType Cont Stop Msg State

serverName :: RegistryName ValueServerType
serverName = Local $ atom "valueServer"

startLink :: Effect (ServerPid ValueServerType)
startLink = do
  crashIfNotStarted
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
