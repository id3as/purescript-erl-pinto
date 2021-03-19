{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, backend =
    "purerl"
, dependencies =
    [ "effect"
    , "erl-atom"
    , "erl-lists"
    , "erl-modules"
    , "erl-process"
    , "erl-tuples"
    , "console"
    , "debug"
    , "erl-test-eunit"
    , "profunctor-lenses"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
