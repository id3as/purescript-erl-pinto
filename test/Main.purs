module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Data.List (List)
import Erl.Test.EUnit (TestF, TestSet, collectTests, runTests, suite, test)

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
    test "info sends output and updates state" do
      pure unit
