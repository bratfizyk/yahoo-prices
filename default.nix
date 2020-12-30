{ mkDerivation, base }:
mkDerivation {
  pname = "FortPolio";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
  ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
