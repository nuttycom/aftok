{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bifunctors, bippy, blake2, blaze-builder, bytestring, cereal
, configurator, containers, cryptonite, directory, either, errors
, free, from-sum, groups, haskoin-core, heaps, hourglass, HsOpenSSL
, hspec, hspec-discover, HStringTemplate, http-client
, http-client-openssl, HUnit, iso8601-time, kan-extensions, lens
, lens-aeson, mime-mail, mtl, network, network-uri, old-locale
, optparse-applicative, postgresql-simple, protobuf, QuickCheck
, relude, safe, semigroupoids, semigroups, smtp-mail, snap
, snap-core, snap-server, snaplet-postgresql-simple, stdenv
, system-filepath, template-haskell, text, thyme, transformers
, unordered-containers, uuid, vector-space, wreq, x509, x509-store
, nix-gitignore
}:
mkDerivation {
  pname = "aftok";
  version = "0.1";
  src = nix-gitignore.gitignoreSource [] ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bifunctors bippy blake2
    blaze-builder bytestring cereal configurator containers cryptonite
    either errors free from-sum groups haskoin-core heaps hourglass
    kan-extensions lens lens-aeson mtl network network-uri old-locale
    postgresql-simple protobuf relude safe semigroupoids semigroups
    smtp-mail system-filepath template-haskell text thyme transformers
    unordered-containers uuid vector-space x509 x509-store
  ];
  executableHaskellDepends = [
    aeson attoparsec base base64-bytestring bippy bytestring cereal
    configurator containers cryptonite directory either errors
    haskoin-core hourglass HsOpenSSL HStringTemplate http-client
    http-client-openssl iso8601-time lens mime-mail mtl network
    network-uri optparse-applicative postgresql-simple protobuf relude
    smtp-mail snap snap-core snap-server snaplet-postgresql-simple
    system-filepath text thyme transformers uuid wreq x509 x509-store
  ];
  testHaskellDepends = [
    aeson attoparsec base bifunctors bippy containers haskoin-core
    hourglass hspec HUnit iso8601-time lens QuickCheck relude
    semigroups text thyme uuid vector-space
  ];
  testToolDepends = [ hspec-discover ];
  description = "The Aftok Collaboration Platform";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
