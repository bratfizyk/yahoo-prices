{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskellPackages;
          devShell = {
            enable = true;
            hlsCheck.enable = true;
          };
        };
        packages.default = self'.packages.yahoo-prices;
      };
    };
}
