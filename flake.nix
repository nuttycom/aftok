{
  description = "The Aftok Collaboration Server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    dbmigrations = {
      url = "github:nuttycom/dbmigrations/74ef9388b45ae73a1d9c737d9644e076fe832672";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dbmigrations-postgresql = {
      url = "github:nuttycom/dbmigrations-postgresql/3c9477e45e923b28d9677dc6291e35bb7c833c28";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dbmigrations-postgresql-simple = {
      url = "github:nuttycom/dbmigrations-postgresql-simple/d51bbc5a0b7d91f7c8a12fc28e5ecbe7ac326221";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bippy = {
      url = "github:aftok/bippy/8166b7e";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lrzhs = {
      url = "github:nuttycom/lrzhs/d29ab9a";
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
      jailbreakUnbreak = pkg:
        final.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));
      unbreak = pkg:
        pkg.overrideAttrs (_: {meta = {};});
    in {
      # Pin resource-pool to 0.2.x for compatibility with snaplet-postgresql-simple
      resource-pool = hfinal.callHackageDirect {
        pkg = "resource-pool";
        ver = "0.2.3.2";
        sha256 = "sha256-Ieg9Vfhoh1Gd5eX5L8usLPHkklCdy6Kwi79ufJSj9I0=";
      } {};
      snaplet-postgresql-simple = jailbreakUnbreak hprev.snaplet-postgresql-simple;
      thyme = unbreak hprev.thyme;
      # Unbreak servant-auth packages for nixpkgs 24.05
      servant-auth = unbreak hprev.servant-auth;
      servant-auth-server = unbreak hprev.servant-auth-server;
      dbmigrations = dbmigrations.defaultPackage.${final.system};
      dbmigrations-postgresql-simple = dbmigrations-postgresql-simple.defaultPackage.${final.system};
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
          buildInputs = [
            pkgs.cabal-install
            lrzhs.packages.${system}.lrzhs_ffi
            pkgs.haskellPackages.ormolu
            (pkgs.haskell.lib.dontCheck dbmigrations-postgresql.defaultPackage.${system})
          ];
          withHoogle = true;
        };

        formatter = pkgs.alejandra;
      }
    );
}
