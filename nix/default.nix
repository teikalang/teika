{ pkgs, doCheck ? true }:

let ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14; in
let inherit (pkgs) lib stdenv ocamlPackages; in

with ocamlPackages; buildDunePackage rec {
  pname = "teika";
  version = "0.0.0-dev";

  src = lib.filterSource {
    src = ./..;
    dirs = [ "src" ];
    files = [ "dune-project" ];
  };

  propagatedBuildInputs = [ menhir menhirLib sedlex ppx_deriving ]
    # checkInputs are here because when cross compiling dune needs test dependencies
    # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
    ++ checkInputs;

  checkInputs = [ alcotest ];
}
