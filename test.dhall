let base = ./spago.dhall

in    base
    ⫽ { sources =
          base.sources # [ "test/**/*.purs" ]
      , dependencies =
          base.dependencies # [ "assert", "console", "erl-test-eunit" ]
      }
