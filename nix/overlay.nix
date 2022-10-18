self: super:
with super; {
  # ocaml-ng = builtins.mapAttrs (_: ocamlVersion: ocamlVersion) super.ocaml-ng;
  # TODO: this is clearly not right, I should be overriding only 4_14
  ocaml-ng = ocaml-ng // (with ocaml-ng; {
    ocamlPackages_5_00 = ocamlPackages_5_00.overrideScope'
      (_: super: {
        dune-rpc = super.dune-rpc.overrideAttrs (_: {
          src = fetchFromGitHub {
            owner = "ocaml";
            repo = "dune";
            rev = "3df932f7f91ea68c3fee789f133b4aa8f9bea807";
            sha256 = "m0HDamHAyOFQlh6P4vbbe8UhARZ4NjwbANInnJ4OZ6U=";
            fetchSubmodules = true;
          };
        });
        dune = super.dune.overrideAttrs (_: {
          src = fetchFromGitHub {
            owner = "ocaml";
            repo = "dune";
            rev = "3df932f7f91ea68c3fee789f133b4aa8f9bea807";
            sha256 = "m0HDamHAyOFQlh6P4vbbe8UhARZ4NjwbANInnJ4OZ6U=";
            fetchSubmodules = true;
          };
        });
        ocaml-lsp = super.ocaml-lsp.overrideAttrs (_: {
          src = fetchFromGitHub {
            owner = "EduardoRFS";
            repo = "ocaml-lsp";
            rev = "72202cd2bb9ff65845b623d20f513f6ed5b82ab8";
            sha256 = "49E7L50i9RZTrQDPmdqyeOaBSXjRo/ijjrsj9oztduM=";
            fetchSubmodules = true;
          };
        });
        jsonrpc = super.jsonrpc.overrideAttrs (_: {
          src = fetchFromGitHub {
            owner = "EduardoRFS";
            repo = "ocaml-lsp";
            rev = "72202cd2bb9ff65845b623d20f513f6ed5b82ab8";
            sha256 = "49E7L50i9RZTrQDPmdqyeOaBSXjRo/ijjrsj9oztduM=";
            fetchSubmodules = true;
          };
        });
      });
  });
}
