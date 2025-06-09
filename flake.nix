{
  description = "The Aftok Collaboration Server";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-23.05;
    flake-utils.url = "github:numtide/flake-utils";
    dbmigrations.url = "github:nuttycom/dbmigrations/74ef9388b45ae73a1d9c737d9644e076fe832672";
    dbmigrations-postgresql.url = "github:nuttycom/dbmigrations-postgresql/3c9477e45e923b28d9677dc6291e35bb7c833c28";
    dbmigrations-postgresql-simple.url = "github:nuttycom/dbmigrations-postgresql-simple/d51bbc5a0b7d91f7c8a12fc28e5ecbe7ac326221";
    bippy.url = "github:aftok/bippy/e809e5a63a251b87d61d55bfc08a5a89c695ef8e";
    lrzhs.url = "github:nuttycom/lrzhs/65ee43717492fe6f2e086c331439b9d61abcdfc7";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    dbmigrations,
    dbmigrations-postgresql,
    dbmigrations-postgresql-simple,
    bippy,
    lrzhs,
    purescript-overlay,
    ...
  }: let

    haskell-overlay = final: prev: hfinal: hprev: let
      jailbreakUnbreak = pkg:
        final.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

      dontCheck = pkg: final.haskell.lib.dontCheck pkg;
    in {
      #base16 = jailbreakUnbreak hprev.base16;
      #murmur3 = jailbreakUnbreak hprev.murmur3;
      #haskoin-core = dontCheck (jailbreakUnbreak hprev.haskoin-core);
      #http-streams = dontCheck hprev.http-streams;
      #openssl-streams = dontCheck hprev.openssl-streams;
      #snap = dontCheck hprev.snap;

      snaplet-postgresql-simple = jailbreakUnbreak hprev.snaplet-postgresql-simple;

      dbmigrations = dbmigrations.defaultPackage;
      dbmigrations-postgresql-simple = dbmigrations-postgresql-simple.defaultPackage;

      aftok = hfinal.callCabal2nix "aftok" ./. {};
    };

    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.extend (haskell-overlay final prev);
    };
  in 
    {
      overlays = {
        default = overlay;
      };
    } 
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
            purescript-overlay.overlays.default
            bippy.overlays.default
            lrzhs.overlays.default
          ];
        };
      in {
        packages = {
          aftok = pkgs.haskellPackages.aftok;
          aftok-server-dockerImage = pkgs.dockerTools.buildImage {
            name = "aftok/aftok-server";
            tag = "latest";
            config = {
              Entrypoint = ["${self.packages.${system}.aftok}/bin/aftok-server" "--conf=/etc/aftok/aftok-server.cfg"];
            };
          };
          default = self.packages.${system}.aftok-server-dockerImage;
        };

        devShells = {
          default = self.devShells.${system}.server;

          server = pkgs.haskellPackages.shellFor {
            name = "server-shell";
            packages = _: [self.packages.${system}.aftok];
            buildInputs = [
              pkgs.cabal-install
              lrzhs.packages.${system}.lrzhs_ffi
              pkgs.haskellPackages.ormolu
            ];
            inputsFrom = builtins.attrValues self.packages.${system};
            withHoogle = true;
          };

          # adapted from example at https://github.com/thomashoneyman/purescript-overlay
          client = pkgs.mkShell {
            name = "client-shell";
            buildInputs = [
              pkgs.purs
              pkgs.spago-unstable
              pkgs.purescript-language-server
              pkgs.purs-tidy-bin.purs-tidy-0_10_0
              pkgs.purs-backend-es
            ];
          };
        };

        formatter = pkgs.alejandra;
      }
    );
}
