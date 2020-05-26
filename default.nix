{ pkgs ?
  import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/fa7445532900f2555435076c1e7dce0684daa01a.tar.gz";
    sha256 = "1hbf7kmbxmd19hj3kz9lglnyi4g20jjychmlhcz4bx1limfv3c3r";
  }) {
    overlays = [
      (self: super:
        let openssl = super.openssl_1_0_2; in {
        ocamlPackages =
          super.ocaml-ng.ocamlPackages_4_09.overrideScope' (self: super: {
            ssl = super.ssl.override {
              inherit openssl;
            };
          });
      })
    ];
  }
}:

with pkgs;

let graphql_ppx = ocamlPackages.callPackage ./graphql_ppx.nix { }; in
stdenv.mkDerivation rec {
  name = "coqbot";
  src = null;
  buildInputs = with ocamlPackages;
    [ # Compiler and dev tools
      ocaml
      findlib
      dune_2
      utop
      ncurses
      merlin
      ocamlformat
      nixfmt
      nodePackages.graphql-cli
      # Direct dependencies
      base
      cohttp
      cohttp-lwt-unix
      hex
      nocrypto
      yojson
      graphql_ppx
      # Publishing
      heroku
    ];

  shellHook = ''
    export OCAMLFORMAT_LOCATION=${ocamlformat}
  '';
}
