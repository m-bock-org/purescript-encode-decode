module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Data.Json.CodecSpec as Data.Json.CodecSpec
import Test.Data.Json.RecordSpec as Data.Json.RecordSpec
import Test.Data.Json.SumSpec as Data.Json.SumSpec
import Test.Data.JsonSpec as Data.JsonSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main =
  runSpecAndExitProcess [ consoleReporter ] do
    Data.JsonSpec.spec
    Data.Json.RecordSpec.spec
    Data.Json.SumSpec.spec
    Data.Json.CodecSpec.spec
