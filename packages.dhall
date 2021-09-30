{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }

  , { metadata = upstream.metadata // { version = "v0.13.2" } }
  }

-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}

let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.3-20210709/packages.dhall sha256:9b07e1fe89050620e2ad7f7623d409f19b5e571f43c2bdb61242377f7b89d941

let additions =
      {
      erl-process =
        { repo = "https://github.com/id3as/purescript-erl-process.git"
        , dependencies =
          [ "console"
          , "prelude"
          , "effect"
          ]
        , version = "afbfa4e7a13c0d55609ff144d49982563fada7f5"
        }
      , erl-kernel =
        { dependencies =
          [ "convertable-options"
          , "datetime"
          , "effect"
          , "either"
          , "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-process"
          , "erl-tuples"
          , "erl-untagged-union"
          , "foldable-traversable"
          , "foreign"
          , "functions"
          , "integers"
          , "maybe"
          , "newtype"
          , "partial"
          , "prelude"
          , "record"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/id3as/purescript-erl-kernel.git"
        , version = "2c1f78a3aa6993e91e342a984c522b87b98bbb2b"
        }
      , erl-untagged-union =
        { dependencies =
          [ "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-tuples"
          , "debug"
          , "foreign"
          , "typelevel-prelude"
          , "maybe"
          , "partial"
          , "prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/id3as/purescript-erl-untagged-union.git"
        , version = "eb7a10c7930c4b99f1a6bfce767daa814d45dd2b"
        }
      , convertable-options =
        { repo = "https://github.com/natefaubion/purescript-convertable-options"
        , dependencies = [ "effect", "maybe", "record" ]
        , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
        }
      }

in  upstream // additions
