{
  description = "The Aftok Collaboration Server";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-23.05;
    flake-utils.url = "github:numtide/flake-utils";
    dbmigrations.url = "github:nuttycom/dbmigrations/74ef9388b45ae73a1d9c737d9644e076fe832672";
    dbmigrations-postgresql.url = "github:nuttycom/dbmigrations-postgresql/3c9477e45e923b28d9677dc6291e35bb7c833c28";
    dbmigrations-postgresql-simple.url = "github:nuttycom/dbmigrations-postgresql-simple/d51bbc5a0b7d91f7c8a12fc28e5ecbe7ac326221";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    dbmigrations,
    dbmigrations-postgresql,
    dbmigrations-postgresql-simple,
  }: let
    overlay = final: prev: let
      jailbreakUnbreak = pkg:
        final.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

      dontCheck = pkg: final.haskell.lib.dontCheck pkg;

      bippy-src = final.fetchFromGitHub {
        owner = "aftok";
        repo = "bippy";
        rev = "6b1234f69eb5a2781644640044f05f046089da89";
        hash = "sha256-F7KrbKKU1BNLxNXu4faGpdYwB0HRC3+opuQB0d4eLhs=";
      };

      haskell-overlay = hfinal: hprev: {
        base16 = jailbreakUnbreak hprev.base16;
        murmur3 = jailbreakUnbreak hprev.murmur3;
        secp256k1 = final.secp256k1;
        haskoin-core = dontCheck (jailbreakUnbreak hprev.haskoin-core);
        http-streams = dontCheck hprev.http-streams;
        openssl-streams = dontCheck hprev.openssl-streams;
        snap = dontCheck hprev.snap;
        bippy = dontCheck (hfinal.callCabal2nix "bippy" bippy-src {});

        snaplet-postgresql-simple = jailbreakUnbreak hprev.snaplet-postgresql-simple;

        dbmigrations = dbmigrations.defaultPackage.${final.system};
        dbmigrations-postgresql-simple = dbmigrations-postgresql-simple.defaultPackage.${final.system};
      };
    in {
      haskellPackages = prev.haskellPackages.extend haskell-overlay;
    };
  in
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [overlay];
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
            buildInputs = [
              hspkgs.ormolu
              (pkgs.haskell.lib.dontCheck dbmigrations-postgresql.defaultPackage.${system})
            ];
            nativeBuildInputs = [
              pkgs.binutils
              pkgs.exa
              pkgs.openssl
              pkgs.postgresql
              pkgs.secp256k1
              pkgs.zlib
            ];
            inputsFrom = builtins.attrValues self.packages.${system};
          };
        };

        formatter = pkgs.alejandra;
      }
    );
}
