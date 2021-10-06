{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, backend = "purerl"
, dependencies =
  [ "effect"
  , "either"
  , "erl-atom"
  , "erl-lists"
  , "erl-modules"
  , "erl-process"
  , "erl-tuples"
  , "datetime"
  , "foreign"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
