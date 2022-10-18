let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.15.3-20220629/packages.dhall
        sha256:48ee9f3558c00e234eae6b8f23b4b8b66eb9715c7f2154864e1e425042a0723b



in upstream

  with erl-process-trans =
    { dependencies =
      [ "datetime"
      , "effect"
      , "either"
      , "erl-kernel"
      , "erl-maps"
      , "erl-process"
      , "erl-tuples"
      , "foreign"
      , "maybe"
      , "partial"
      , "prelude"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      , "unsafe-coerce"
      ]
    , repo = "ssh://git@github.com/id3as/purescript-erl-process-trans"
    , version = "2e864358a89da996d37f579b739f64d355f9ff83"
    }
