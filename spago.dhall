{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, backend = "purerl"
, dependencies =
  [ "datetime"
  , "effect"
  , "either"
  , "erl-atom"
  , "erl-lists"
  , "erl-maps"
  , "erl-modules"
  , "erl-process"
  , "erl-tuples"
  , "erl-untagged-union"
  , "foreign"
  , "functions"
  , "maybe"
  , "partial"
  , "prelude"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
