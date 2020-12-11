let
  erlangReleases = builtins.fetchGit {
    name = "nixpkgs-nixerl";
    url = "https://github.com/nixerl/nixpkgs-nixerl";
    rev = "6321e5b8b6cfe4c13307a2d2093c4b6243e6ad53";
  }; #builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.15-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "e5f945b13b3f6a39ec9fbb66c9794b277dc32aa1";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "b3f10cd33107f220e4328f0222d3d026bf4f5f99";
    };

  id3asPackages =
    builtins.fetchGit {
      name = "id3as-packages";
      url = "git@github.com:id3as/nixpkgs-private.git";
      ref = "v3";
      rev = "3ebb89abfd4dc05885e90ad6d2980ea1b38f9cfe";
    };

  mozillaPackages =
    builtins.fetchGit {
      name = "nixpkgs-mozilla";
      url = https://github.com/mozilla/nixpkgs-mozilla/;
      # commit from: 2020-04-14
      rev = "e912ed483e980dfb4666ae0ed17845c4220e5e7c";
  };

  oxidizedPackages =
    builtins.fetchGit {
      name = "id3as-oxidized-packages";
      url = "git@github.com:id3as/oxidized.git";
      rev = "08a98ebb798609746d50202026a4b170dcd9be85";
    };

  etwasPackages =
    builtins.fetchGit {
      name = "id3as-etwas-packages";
      url = "https://github.com/id3as/etwas";
      rev = "289d841fa2fffccca266407764dc619cfae6a2fb";
    };

  purerlSupport =
    builtins.fetchGit {
      name = "purerl-support-packages";
      url = "git@github.com:id3as/nixpkgs-purerl-support.git";
      rev = "47a8bd6ff017dad2208f10dddf91f6f3258a09be";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
        (import purerlSupport)
        (import id3asPackages)
        (import oxidizedPackages)
        (import mozillaPackages)
        (import "${etwasPackages}/overlay.nix")
      ];
    };

  rust =
    ((nixpkgs.rustChannelOf { channel = "1.48.0"; }).rust.override   {
      extensions = [
        "rust-src"
        "rls-preview"
        "rust-analysis"
        "rustfmt-preview"
        "clippy-preview"
      ];
    });

in

with nixpkgs;

let
    inherit (stdenv.lib) optionals;
in

mkShell {
  buildInputs = with pkgs; [


    (nixerl.erlang-23-0-4.erlang.override { wxSupport = false; })
    (nixerl.erlang-23-0-4.rebar3.override { erlang = (nixpkgs.nixerl.erlang-23-0-4.erlang.override { wxSupport = false; }); })
    (purerl-support.erlang_ls-0-5-1.override { erlang = (nixpkgs.nixerl.erlang-23-0-4.erlang.override { wxSupport = false; }); })

    # Our nativedeps environment
    id3as.nd-env

    # Purescript - we use a specific version rather than
    # whatever the latest is exposed via nixpkgs
    purerl-support.purescript-0-13-8
    purerl-support.spago-0-16-0
    purerl-support.dhall-json-1-5-0

    # Purerl backend for purescript
    purerl.purerl-0-0-7

    # Client stuff
    nodejs
    nodePackages.npm
    nodePackages.parcel-bundler

    # Rust stuff!
    rust

    prometheus

    # Rust is broken
    zlib

  ]
  ++ optionals stdenv.isLinux [
    iproute
  ]
  ++ optionals stdenv.isDarwin [
    coreutils
    bash
    tmux
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${zlib.out}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
  '';
}
