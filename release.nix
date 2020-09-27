let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

	haskoin-core = pkgs.fetchFromGitHub {
		owner = "haskoin";
		repo = "haskoin-core";
		rev = "202dd677f377c82aac23224f5b2fedad57db5574";
		sha256 = "1mykgak4flrl0hx0xkll4rdjlfvr91r5l9rlhbyc6axl40487dqn";
	};

  bippy = pkgs.fetchFromGitHub { 
    owner = "aftok";
    repo = "bippy";
    rev = "1f373039ff22d51c54b41cda57e688e74d00a642";
    sha256 = "06rf3vgd20g4lsl7y2yi843k1mm5hvvf3vnmqi5i7m4ll9napchp";
  };

	# haskellPackages = pkgs.haskellPackages.override {
	# 	overrides = self: super: {
	# 		haskoin-core = pkgs.haskellPackages.callCabal2nix "haskoin-core" haskoin-core {};
	# 		bippy = pkgs.haskellPackages.callCabal2nix "bippy" bippy {};
	# 	};
	# };
in
  haskellPackages.callPackage ./default.nix { }
