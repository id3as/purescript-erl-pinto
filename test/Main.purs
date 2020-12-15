module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Data.List (List)
import Erl.Test.EUnit (TestF, TestSet, collectTests, runTests, suite, test)
import Pinto.GenServer (InitFn, ServerRunning(..))
import Pinto.GenServer as GS
import Test.Assert (assert, assertEqual)

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


data State = TestState
data Cont = TestCont
data Msg = TestMsg

testStartLinkAnonymous =
  test "Can start an anonymous GenServer" do
    slRes <- GS.startLink Nothing init
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
