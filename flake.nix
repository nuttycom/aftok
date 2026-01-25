{
  description = "The Aftok Collaboration Server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    dbmigrations = {
      url = "github:haskell-github-trust/dbmigrations/d870aa2bdc6ac219bfdd182bcada3a9534dc23a8";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dbmigrations-postgresql = {
      url = "github:nuttycom/dbmigrations-postgresql/e7427642c12aa5f2d9bfedd268cfc8dcf78f314e";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dbmigrations-postgresql-simple = {
      url = "github:nuttycom/dbmigrations-postgresql-simple/f26b9e1ab27b9ff8f269c6541a7606bcbc22e02a";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bippy = {
      url = "github:aftok/bippy/1108583";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lrzhs = {
      url = "github:nuttycom/lrzhs/657f258";
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
    ...
  }: let
    haskell-overlay = final: prev: hfinal: hprev: let
      unbreak = pkg:
        pkg.overrideAttrs (_: {meta = {};});
    in {
      thyme = unbreak hprev.thyme;
      # Unbreak servant-auth packages for nixpkgs 24.11
      servant-auth = unbreak hprev.servant-auth;
      servant-auth-server = unbreak hprev.servant-auth-server;
      dbmigrations = dbmigrations.packages.${final.stdenv.hostPlatform.system}.default;
      dbmigrations-postgresql = dbmigrations-postgresql.packages.${final.stdenv.hostPlatform.system}.default;
      dbmigrations-postgresql-simple = dbmigrations-postgresql-simple.packages.${final.stdenv.hostPlatform.system}.default;
      aftok = hfinal.callCabal2nix "aftok" ./. {};
    };

    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.extend (haskell-overlay final prev);
    };
  in
    {
      overlays.default = overlay;
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
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

        devShells.default = pkgs.haskellPackages.shellFor {
          name = "aftok-server-shell";
          packages = p: [p.aftok];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.pkg-config
            pkgs.haskellPackages.ormolu
            pkgs.haskellPackages.dbmigrations-postgresql
          ];
          buildInputs = [
            lrzhs.packages.${system}.lrzhs_ffi
          ];
          withHoogle = true;
        };

        formatter = pkgs.alejandra;
      }
    );
}
