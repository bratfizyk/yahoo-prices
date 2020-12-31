{ mkDerivation, base, bytestring, Cabal, cassava, lens, wreq }:
mkDerivation {
  pname = "yahoo-price";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring Cabal cassava lens wreq
  ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
