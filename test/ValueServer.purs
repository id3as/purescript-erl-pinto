module Test.ValueServer
  ( Msg
  , ValueServerPid
  , getValue
  , setValue
  , setValueAsync
  , startLink
  , stop
  , testValueServer
  ) where

import Prelude

import Control.Monad.Free (Free)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Process.Raw (class HasPid)
import Erl.Test.EUnit (TestF, test)
import Pinto.GenServer2 (InitFn, InitResult(..), ServerPid, ServerType)
import Pinto.GenServer2 as GS
import Erl.ProcessT (ProcessM)
import Pinto.Types (RegistryName(..), RegistryReference(..), crashIfNotStarted)
import Test.Assert (assertEqual)

data Msg = SetValue Int

type State = { value :: Int }

type ValueServerType = ServerType State (ProcessM Msg)

newtype ValueServerPid = ValueServerPid (ServerPid State (ProcessM Msg))

-- Only surface the raw pid, don't implement HasProcess - we don't want folks sending us messages using our Info
-- type
derive newtype instance valueServerPidHasPid :: HasPid ValueServerPid

serverName :: RegistryName ValueServerType
serverName = Local $ atom "valueServer"

startLink :: Effect ValueServerPid
startLink = do
  ValueServerPid
    <$> crashIfNotStarted
    <$> (GS.startLink' { serverName, init })
  where
  init :: InitFn State (ProcessM Msg)
  init =
    let
      state = { value: 0 }
    in
      pure $ InitOk state

getValue :: Effect Int
getValue = GS.call (ByName serverName) impl
  where
  impl _from state@{ value } = pure $ GS.reply value state

setValue :: Int -> Effect Int
setValue n = GS.call (ByName serverName) impl
  where
  impl _from state@{ value } = pure $ GS.reply value state { value = n }

setValueAsync :: Int -> Effect Unit
setValueAsync n = GS.cast (ByName serverName) impl
  where
  impl state = pure $ GS.return state { value = n }

stop :: Effect Unit
stop = GS.stop (ByName serverName)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
testValueServer :: Free TestF Unit
testValueServer =
  test "Interaction with gen_server with closed API" do
    void $ startLink
    void $ setValue 42
    v1 <- setValue 43
    v2 <- getValue
    setValueAsync 50
    v3 <- getValue
    stop
    assertEqual { actual: v1, expected: 42 }
    assertEqual { actual: v2, expected: 43 }
    assertEqual { actual: v3, expected: 50 }
    pure unit
