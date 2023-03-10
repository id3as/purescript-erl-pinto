
let
  pinnedNixHash = "7c3d4d3af8e9319ccd2a74c31cf247b0fcd08bc2";
  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "${pinnedNixHash}";
    };

  erlangReleases =
    builtins.fetchGit {
      name = "nixpkgs-nixerl";
      url = "https://github.com/id3as/nixpkgs-nixerl.git";
      rev = "9c74cc241f7a13e2ea8ebe765cb3959501c5c404";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "1d59def257ad38fcbc17782591e5a21df3fd2557";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
      ];
    };

  easy-ps = import
    (nixpkgs.pkgs.fetchFromGitHub {
      ## Temporarily on Fabrizio's fork to get spago-next
      owner = "f-f";
      repo = "easy-purescript-nix";
      rev = "880d958dd9909872629e3f040da2b4ec2ef6e990";
      sha256 = "sha256-+6lTkonRAY6KvDIweRU5DeuWMryFELJTEzg8P2OSjgM=";
    }) { pkgs = nixpkgs; };

  erlang = nixpkgs.nixerl.erlang-25-0;
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    erlang.erlang
    erlang.rebar3
    erlang.erlang-ls

    # Purescript itself
    easy-ps.purs-0_15_7
    easy-ps.spago
    easy-ps.psa
    easy-ps.purescript-language-server
    easy-ps.purs-tidy
    purerl.purerl-0-0-19
   ];
}
