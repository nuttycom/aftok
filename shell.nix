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
    pkgs.cacert
    pkgs.ghc
    pkgs.git
    pkgs.nodejs
    pkgs.pkg-config
    pkgs.purescript
    haskellPackages.brittany
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.ormolu
  ] ++ libInputs;

  shellHook = ''
    export PATH="./node_modules/.bin:$PATH"
  '';

  SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  LD_LIBRARY_PATH = "${pkgs.stdenv.lib.makeLibraryPath libInputs}";
}
