
let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.18-devel.tar.gz;

  pinnedNixHash = "e5f945b13b3f6a39ec9fbb66c9794b277dc32aa1";
  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "${pinnedNixHash}";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "16582722c40f4c1a65c15f23e5f2438c6905981f";
    };

  purerlSupport =
    builtins.fetchGit {
      name = "purerl-support-packages";
      url = "git@github.com:id3as/nixpkgs-purerl-support.git";
      rev = "52926a56da6a8c526c403d26feaf52cc5f87a5d0";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
        (import purerlSupport)
      ];
    };

  erlangChannel = nixpkgs.nixerl.erlang-23-2-1.overrideScope' (self: super: {
    erlang = super.erlang.override {
      wxSupport = false;
    };
  });

  pls = nixpkgs.nodePackages.purescript-language-server.override {
    version = "0.15.4";
    src = builtins.fetchurl {
      url = "https://registry.npmjs.org/purescript-language-server/-/purescript-language-server-0.15.4.tgz";
    };
  };

  pose = nixpkgs.nodePackages.purty.override {
      name = "prettier-plugin-purescript";
      packageName = "prettier-plugin-purescript";
      version = "1.11.1";
      src = builtins.fetchurl {
        url = "https://registry.npmjs.org/@rowtype-yoga/prettier-plugin-purescript/-/prettier-plugin-purescript-1.11.1.tgz";
      };
      meta = {
        description = "Hacked in Purescript Prettier Plugin";
        license = "MIT";
      };
    };
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    erlangChannel.erlang
    erlangChannel.rebar3
    erlangChannel.erlang-ls

    # Purescript itself
    purerl-support.purescript-0-14-4
    purerl-support.spago-0-20-3
    purerl-support.dhall-json-1-5-0
    purerl-support.psa-0-8-2

    # Purerl backend for purescript
    purerl.purerl-0-0-12
    
    # The Language server for purescript
    pls

    # The current hotness for code formatting
    pose

   ];
}
