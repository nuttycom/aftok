{
  description = "The Aftok Collaboration Server";

  nixConfig.allow-import-from-derivation = true;

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    dbmigrations = {
      url = "github:haskell-github-trust/dbmigrations/6d641f169b60ecb9e9de6e7e82d4bafbcaac62ff";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dbmigrations-postgresql = {
      url = "github:nuttycom/dbmigrations-postgresql/5a6a69e2b1134fdef18242091e9ee8d9eea48328";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dbmigrations-postgresql-simple = {
      url = "github:nuttycom/dbmigrations-postgresql-simple/ab0ea9d7ac53359642239989ea5257c7b93dda37";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bippy = {
      url = "github:aftok/bippy/1108583";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lrzhs = {
      url = "github:nuttycom/lrzhs/51838a296afc0f0b8c8ec7cf0018dc12f989ecc5";
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
      aftok = hfinal.callCabal2nix "aftok" ./core {};
      aftok-api = hfinal.callCabal2nix "aftok-api" ./api {};
      aftok-executables = hfinal.callCabal2nix "aftok-executables" ./executables {};
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
          aftok-api = pkgs.haskellPackages.aftok-api;
          aftok-executables = pkgs.haskellPackages.aftok-executables;
          templates = pkgs.runCommand "aftok-templates" {} ''
            mkdir -p $out/opt/aftok/server/templates
            cp ${./executables/server/templates}/* $out/opt/aftok/server/templates/
          '';
          migrations = pkgs.runCommand "aftok-migrations" {} ''
            mkdir -p $out/opt/aftok/migrations
            cp ${./migrations}/* $out/opt/aftok/migrations/
          '';
          dockerImage = pkgs.dockerTools.buildImage {
            name = "aftok/aftok-server";
            tag = "latest";
            copyToRoot = pkgs.buildEnv {
              name = "aftok-server-root";
              paths = [
                self.packages.${system}.templates
                self.packages.${system}.migrations
                pkgs.cacert  # CA certificates for HTTPS connections
              ];
              pathsToLink = [ "/opt" "/etc" ];
            };
            config = {
              Entrypoint = ["${self.packages.${system}.aftok-executables}/bin/aftok-server" "--conf=/etc/aftok/aftok-server.cfg"];
              Env = [
                "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
              ];
            };
          };
          default = self.packages.${system}.dockerImage;
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          name = "aftok-server-shell";
          packages = p: [p.aftok p.aftok-api p.aftok-executables];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.pkg-config
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.ormolu
            pkgs.haskellPackages.hspec-discover
            pkgs.haskellPackages.dbmigrations-postgresql
            (pkgs.writeShellScriptBin "format" ''
              find core api executables -name '*.hs' -exec ormolu --mode inplace {} +
            '')
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
