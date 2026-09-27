{
  description = "Nix Flake";

  inputs = {
    nixpkgs.url = "github:anmonteiro/nix-overlays";
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      nix-filter,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.makePkgs { inherit system; };
        teika = pkgs.callPackage ./nix/teika.nix {
          inherit nix-filter;
          doCheck = true;
        };
      in
      {
        packages = {
          inherit teika;
          default = teika;
        };

        devShells.default = import ./nix/shell.nix {
          inherit pkgs teika;
        };
      }
    );
}
