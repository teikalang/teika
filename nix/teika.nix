{
  pkgs,
  doCheck ? true,
  nix-filter,
}:

let
  inherit (pkgs) ocaml-ng;
in
with ocaml-ng.ocamlPackages_5_5;
buildDunePackage {
  pname = "teika";
  version = "0.0.0-dev";

  src =
    with nix-filter.lib;
    filter {
      root = ./..;
      include = [
        "dune-project"
        "teika"
      ];
      exclude = [ ];
    };

  inherit doCheck;

  propagatedBuildInputs = [
    alcotest
    eio
    eio_main
    lsp
    melange
    menhir
    menhirLib
    ppx_deriving
    ppx_sexp_conv
    sedlex
    zarith
  ];
}
