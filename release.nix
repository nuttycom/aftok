let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { config = { allowBroken = true; }; };

  #haskoin-core 0.15.0
  haskoin-core = pkgs.fetchFromGitHub {
    owner = "haskoin";
    repo = "haskoin-core";
    rev = "202dd677f377c82aac23224f5b2fedad57db5574";
    sha256 = "1mykgak4flrl0hx0xkll4rdjlfvr91r5l9rlhbyc6axl40487dqn";
  };

  #secp256k1-haskell
  secp256k1-haskell = pkgs.fetchFromGitHub {
    owner = "haskoin";
    repo = "secp256k1-haskell";
    rev = "5c6bc5601ee2cd6725789df3edd052b5697ecbc6";
    sha256 = "0rvch2yvhx7sgqipggd5acqmg6ckkp1rs3z3gc3a0ii5vl62mvlh";
  };

  #bippy-0.2.0.0
  bippy = pkgs.fetchFromGitHub { 
    owner = "aftok";
    repo = "bippy";
    rev = "1f373039ff22d51c54b41cda57e688e74d00a642";
    sha256 = "06rf3vgd20g4lsl7y2yi843k1mm5hvvf3vnmqi5i7m4ll9napchp";
  };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      secp256k1-haskell = self.callCabal2nix "secp256k1-haskell" secp256k1-haskell {};
      haskoin-core = self.callCabal2nix "haskoin-core" haskoin-core {};
      bippy = self.callCabal2nix "bippy" bippy {};
    };
  };
in
  #haskellPackages.callPackage ./default.nix { }
  haskellPackages.bippy
