{ nixpkgs ? import ./pkgs.nix {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base bytestring cassava hspec lens time vector wreq
  ];

  ghc = pkgs.haskell.packages.ghc884.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
  ];
in
pkgs.stdenv.mkDerivation {
  name = "snadbox-haskell-workspace";
  buildInputs = nixPackages;
}