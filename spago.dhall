{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, backend = "purerl"
, dependencies =
  [ "console"
  , "contravariant"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "erl-atom"
  , "erl-kernel"
  , "erl-lists"
  , "erl-maps"
  , "erl-modules"
  , "erl-process"
  , "erl-tuples"
  , "erl-untagged-union"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "lists"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "run"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "uncurried-transformers"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
