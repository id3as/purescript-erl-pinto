let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.5-20220204-2/packages.dhall sha256:bf284d597ad053b43591b964a52aa0f41ed12a576c3efde85ba999ad65072fc9

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

      }




in (upstream // additions)
