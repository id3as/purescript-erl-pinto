let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.15.3-20220629/packages.dhall
        sha256:48ee9f3558c00e234eae6b8f23b4b8b66eb9715c7f2154864e1e425042a0723b

let additions =
      { run =
        { repo = "https://github.com/natefaubion/purescript-run"
        , dependencies =
          [ "either"
          , "free"
          , "maybe"
          , "newtype"
          , "prelude"
          , "tailrec"
          , "tuples"
          , "type-equality"
          , "unsafe-coerce"
          , "variant"
          , "profunctor"
          , "effect"
          , "typelevel-prelude"
          ]
        , version = "abec7c343e92154d44b9dafd52b91ee82d32a870"
        }
      , convertable-options =
        { repo = "https://github.com/natefaubion/purescript-convertable-options"
        , dependencies = [ "effect", "maybe", "record" ]
        , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
        }
      , uncurried-transformers =
        { repo = "https://github.com/PureFunctor/purescript-uncurried-transformers.git"
        , dependencies = ["effect"]
        , version = "221fe4b5f404cf9b8149605f1d054946f4f0ab0d"
        }
      }




in (upstream // additions)
