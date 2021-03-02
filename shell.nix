{ nixpkgs ? import ./pkgs.nix {} }:
let
  inherit (nixpkgs) pkgs;
in
pkgs.stdenv.mkDerivation {
  name = "yahoo-prices-shell";
  buildInputs = with pkgs; [ (import ./release.nix) ];
}