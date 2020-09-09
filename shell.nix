let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  inherit (pkgs) haskellPackages;

  libInputs = [
    pkgs.postgresql
    pkgs.openssl
    pkgs.secp256k1
    pkgs.zlib
  ];
in
pkgs.stdenv.mkDerivation{
  name = "aftok";

  buildInputs = [
    pkgs.ghc
    pkgs.git
    pkgs.pkg-config
    haskellPackages.cabal-install
    haskellPackages.ghcid
  ] ++ libInputs;

  LD_LIBRARY_PATH = "${pkgs.stdenv.lib.makeLibraryPath libInputs}";
}
