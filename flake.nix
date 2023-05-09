{
  description = "The Aftok Collaboration Server";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-22.11;
    flake-utils.url = "github:numtide/flake-utils";
    dbmigrations.url = "github:nuttycom/dbmigrations/74ef9388b45ae73a1d9c737d9644e076fe832672";
    dbmigrations-postgresql.url = "github:nuttycom/dbmigrations-postgresql/3c9477e45e923b28d9677dc6291e35bb7c833c28";
    dbmigrations-postgresql-simple.url = "github:nuttycom/dbmigrations-postgresql-simple/d51bbc5a0b7d91f7c8a12fc28e5ecbe7ac326221";
    thyme.url = "github:nuttycom/thyme/14422f2dd5ba369cf68b1c42363dcb6f92860ccc";
    lrzhs.url = "github:nuttycom/lrzhs/b10c8cc245f2353dfe3f5cbd6e231082a23ced7d";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    dbmigrations,
    dbmigrations-postgresql,
    dbmigrations-postgresql-simple,
    thyme,
    lrzhs
  }: let
    overlay = final: prev: let
      jailbreakUnbreak = pkg:
        final.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

      dontCheck = pkg: final.haskell.lib.dontCheck pkg;

      bippy-src = final.fetchFromGitHub {
        owner = "aftok";
        repo = "bippy";
        rev = "1f373039ff22d51c54b41cda57e688e74d00a642";
        hash = "sha256-F7KrbKKU1BNLxNXu4faGpdYwB0HRC3+opuQB0d4eLhs=";
      };

      secp256k1-haskell-src = final.fetchFromGitHub {
        owner = "haskoin";
        repo = "secp256k1-haskell";
        rev = "3df963ab6ae14ec122691a97af09a7331511a387";
        hash = "sha256-XrjiqCC7cNTFib78gdMPGNettAkwAxQlbC/n+/mRFt4=";
      };

      HsOpenSSL-src = final.fetchFromGitHub {
        owner = "haskell-cryptography";
        repo = "HsOpenSSL";
        rev = "094f7ef6c9ef4dc3eea56802382b1d50c99572d6";
        hash = "sha256-l2TZzGehqvQ3cB9UlfEg5SuiLkvu6ncwqV4Kdto1zOc=";
      };

      haskell-overlay = hfinal: hprev: {
        base16 = jailbreakUnbreak hprev.base16;
        murmur3 = jailbreakUnbreak hprev.murmur3;
        secp256k1 = final.secp256k1;
        secp256k1-haskell = hfinal.callCabal2nix "secp256k1-haskell" secp256k1-haskell-src {};
        haskoin-core = dontCheck (jailbreakUnbreak hprev.haskoin-core);
        bippy = dontCheck (hfinal.callCabal2nix "bippy" bippy-src {});

        HsOpenSSL = hfinal.callCabal2nix "HsOpenSSL" HsOpenSSL-src {};
        snaplet-postgresql-simple = jailbreakUnbreak hprev.snaplet-postgresql-simple;

        dbmigrations = dbmigrations.defaultPackage.${final.system};
        dbmigrations-postgresql-simple = dbmigrations-postgresql-simple.defaultPackage.${final.system};

        lrzhs = lrzhs.packages.${final.system}.default;
      };
    in {
      haskellPackages = prev.haskellPackages.extend haskell-overlay;
    };
  in
    flake-utils.lib.eachSystem [flake-utils.lib.system.x86_64-linux] (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [thyme.overlays.${system}.default overlay];
        };

        hspkgs = pkgs.haskellPackages;
      in {
        packages = {
          aftok-server = hspkgs.callCabal2nix "aftok" ./. {};
          aftok-server-dockerImage = pkgs.dockerTools.buildImage {
            name = "aftok/aftok-server";
            tag = "latest";
            config = {
              Entrypoint = ["${self.packages.${system}.aftok-server}/bin/aftok-server" "--conf=/etc/aftok/aftok-server.cfg"];
            };
          };
          default = self.packages.${system}.aftok-server-dockerImage;
        };

        devShells = {
          default = pkgs.mkShell {
            inputsFrom =
              builtins.attrValues self.packages.${system};

            buildInputs = [
              lrzhs.packages.${system}.lrzhs_ffi
              pkgs.secp256k1
              pkgs.zlib
              pkgs.openssl
            ];

            nativeBuildInputs = [
              hspkgs.ormolu
              (pkgs.haskell.lib.dontCheck dbmigrations-postgresql.defaultPackage.${system})
              pkgs.alejandra
              pkgs.binutils
              pkgs.postgresql
            ];

            LD_LIBRARY_PATH = [
              "${pkgs.zlib}/lib/"
              "${pkgs.openssl}/lib/"
              "${pkgs.secp256k1}/lib/"
            ];
          };
        };

        formatter = pkgs.alejandra;
      }
    );
}
