{ mkDerivation, base, bytestring, Cabal, cassava, hspec, lens, stdenv, time, vector, wreq }:
mkDerivation {
  pname = "yahoo-price";
  version = "0.1.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  executableHaskellDepends = [
    base bytestring Cabal cassava lens hspec time vector wreq
  ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
