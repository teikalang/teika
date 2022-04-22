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
        not-named-yet = pkgs.callPackage ./nix { doCheck = true; };
      in
      rec {
        packages = { inherit not-named-yet; };
        devShell = import ./nix/shell.nix { inherit pkgs not-named-yet; };
      });
}
