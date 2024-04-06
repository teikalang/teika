self: super:
with super; {
  ocaml-ng = ocaml-ng // (with ocaml-ng; {
    ocamlPackages_5_1 = ocamlPackages_5_1.overrideScope'
      (_: super: {
        ocaml-lsp = super.ocaml-lsp.overrideAttrs (_: {
          src = fetchFromGitHub {
            owner = "ocaml";
            repo = "ocaml-lsp";
            rev = "922a726d2dcd816263c34df7d8d0f44fd4700be2";
            sha256 = "9Mt5hD3W0hDyIcPos9anTFRW7RHjEg3+2inj6jOY2S4=";
            fetchSubmodules = true;
          };
        });
        jsonrpc = super.jsonrpc.overrideAttrs (_: {
          src = fetchFromGitHub {
            owner = "ocaml";
            repo = "ocaml-lsp";
            rev = "922a726d2dcd816263c34df7d8d0f44fd4700be2";
            sha256 = "9Mt5hD3W0hDyIcPos9anTFRW7RHjEg3+2inj6jOY2S4=";
            fetchSubmodules = true;
          };
        });
      });
  });
}
