module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (Atom, atom)
import Erl.Data.List (List)
import Erl.Process (Process(..), (!))
import Erl.Process.Raw (Pid)
import Erl.Test.EUnit (TestF, TestSet, collectTests, runTests, suite, test)
import Pinto.GenServer (CastResult(..), InitFn, ServerRunning(..))
import Pinto.GenServer as GS
import Pinto.Types (RegistryName(..))
import Test.Assert (assert, assertEqual)
import Unsafe.Coerce (unsafeCoerce)

foreign import filterSasl :: Effect  Unit

main :: Effect Unit
main =
  let _ = unsafePerformEffect filterSasl
  in
    void $ runTests do
      genServerSuite

genServerSuite :: Free TestF Unit
genServerSuite =
  suite "Pinto genServer test" do
    testStartLinkAnonymous
    testStartLinkNamed
    testHandleInfo


data State = TestState
data Cont = TestCont
data Msg = TestMsg

testStartLinkAnonymous =
  test "Can start an anonymous GenServer" do
    slRes <- GS.startLink $ GS.mkSpec init
    let
      worked = case slRes of
        Right pid -> true
        Left reason -> false
    assert worked
    pure unit

    where
      init :: forall cont msg. InitFn State cont msg
      init = do
        self <- GS.self

        let _ = spy "Got self" self

        pure $ Right $ InitOk TestState

testStartLinkNamed =
  test "Can start an anonymous GenServer" do
    slRes <- GS.startLink $ (GS.mkSpec init) { name = Just (Local (atom "foo")) }
    let
      worked = case slRes of
        Right pid -> true
        Left reason -> false
    assert worked
    pure unit

    where
      init :: forall cont msg. InitFn State cont msg
      init = do
        self <- GS.self

        let _ = spy "Got self" self

        pure $ Right $ InitOk TestState

testHandleInfo =
  test "HandleInfo handler receives message" do
    slRes <- GS.startLink $ (GS.mkSpec init) { handleInfo = Just handleInfo }

    worked <- case slRes of
        Right pid -> do
            (unsafeCoerce pid :: Process Msg) ! TestMsg
            pure true
        Left reason ->
            pure false

    assert worked
    pure unit

    where
      init :: forall cont msg. InitFn State cont msg
      init = do
        self <- GS.self

        let _ = spy "Got self" self

        pure $ Right $ InitOk TestState

      handleInfo msg state = do

        let _ = spy "Got message" msg

        pure $ NoReply state
