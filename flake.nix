{
  description = "Nix Flake";

  inputs = {
    nixpkgs.url = "github:anmonteiro/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:

      let
        pkgs = nixpkgs.legacyPackages."${system}";
        teika = pkgs.callPackage ./nix { doCheck = true; };
      in
      rec {
        packages = { inherit teika; };
        devShell = import ./nix/shell.nix { inherit pkgs teika; };
      });
}
