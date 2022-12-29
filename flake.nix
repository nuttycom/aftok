{
  description = "The Aftok Collaboration Server";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-22.11;
    flake-utils.url = "github:numtide/flake-utils";
    dbmigrations.url =
      "github:nuttycom/dbmigrations/74ef9388b45ae73a1d9c737d9644e076fe832672";
    dbmigrations-postgresql.url =
      "github:nuttycom/dbmigrations-postgresql/3c9477e45e923b28d9677dc6291e35bb7c833c28";
    dbmigrations-postgresql-simple.url =
      "github:nuttycom/dbmigrations-postgresql-simple/d51bbc5a0b7d91f7c8a12fc28e5ecbe7ac326221";
    secp256k1-haskell.url =
      "github:nuttycom/secp256k1-haskell/610780ee4c0c74bb1b66169d863a47e5fb49211e";
    snaplet-postgresql-simple.url =
      "github:nuttycom/snaplet-postgresql-simple/c314c6f64bc00d05dab9630f30f269740da6ea9e";
    thyme.url = "github:nuttycom/thyme/6fc5b0f5da0a20007db8fac8c225a85bb780aee1";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , dbmigrations
    , dbmigrations-postgresql
    , dbmigrations-postgresql-simple
    , secp256k1-haskell
    , snaplet-postgresql-simple
    , thyme
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };

      bippy-src = pkgs.fetchFromGitHub {
        owner = "aftok";
        repo = "bippy";
        rev = "1f373039ff22d51c54b41cda57e688e74d00a642";
        hash = "sha256-F7KrbKKU1BNLxNXu4faGpdYwB0HRC3+opuQB0d4eLhs=";
      };

      HsOpenSSL-src = pkgs.fetchFromGitHub {
        owner = "haskell-cryptography";
        repo = "HsOpenSSL";
        rev = "094f7ef6c9ef4dc3eea56802382b1d50c99572d6";
        hash = "sha256-l2TZzGehqvQ3cB9UlfEg5SuiLkvu6ncwqV4Kdto1zOc=";
      };

      jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

      haskell-overlay = final: prev: {
        base16 = jailbreakUnbreak prev.base16;
        bippy = pkgs.haskell.lib.dontCheck (prev.callCabal2nix "bippy" bippy-src { });
        haskoin-core = pkgs.haskell.lib.dontCheck (jailbreakUnbreak prev.haskoin-core);
        murmur3 = jailbreakUnbreak prev.murmur3;
        secp256k1-haskell = secp256k1-haskell.defaultPackage.${system};
        thyme = thyme.defaultPackage.${system};

        HsOpenSSL = prev.callCabal2nix "HsOpenSSL" HsOpenSSL-src { };
        openssl-streams = prev.openssl-streams.override {
          HsOpenSSL = final.HsOpenSSL;
        };
        http-streams = prev.http-streams.override {
          HsOpenSSL = final.HsOpenSSL;
        };
        snap-server = prev.snap-server.override {
          HsOpenSSL = final.HsOpenSSL;
          openssl-streams = final.openssl-streams;
        };
        snap = prev.snap.override {
          snap-server = final.snap-server;
        };
        snaplet-postgresql-simple = snaplet-postgresql-simple.defaultPackage.${system}.override {
          snap = final.snap;
        };

        dbmigrations = dbmigrations.defaultPackage.${system};
        dbmigrations-postgresql-simple = dbmigrations-postgresql-simple.defaultPackage.${system};
      };

      hspkgs = pkgs.haskellPackages.override {
        overrides = haskell-overlay;
      };

      aftok = hspkgs.callCabal2nix "aftok" ./. { };

      aftok-server-dockerImage = pkgs.dockerTools.buildImage {
        name = "aftok-server";
        config = {
          Entrypoint = [ "${aftok}/bin/aftok-server" "--conf=/etc/aftok/aftok-server.cfg" ];
        };
      };
    in
    {
      packages = {
        aftok-server = aftok;
        aftok-server-dockerImage = aftok-server-dockerImage;
      };

      defaultPackage = aftok-server-dockerImage;

      devShell = pkgs.mkShell {
        buildInputs = [
          hspkgs.cabal-install
          hspkgs.ghc
          hspkgs.ghcid
          hspkgs.ormolu
          (pkgs.haskell.lib.dontCheck dbmigrations-postgresql.defaultPackage.${system})
          pkgs.nixpkgs-fmt
        ];
        nativeBuildInputs = [
          pkgs.binutils
          pkgs.openssl
          pkgs.postgresql
          pkgs.secp256k1
          pkgs.zlib
        ];
        inputsFrom = builtins.attrValues self.packages.${system};
      };
    }
    );
}
