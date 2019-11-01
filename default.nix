{ pkgs ?
  import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/c0e56afddbcf6002e87a5ab0e8e17f381e3aa9bd.tar.gz";
    sha256 = "1zg28j760qgjncqrf4wyb7ijzhnz0ljyvhvv87m578c7s84i851l";
  }) {}
}:

with pkgs;

stdenv.mkDerivation rec {
  name = "coqbot";
  src = null;
  buildInputs = with ocamlPackages;
    [ # Compiler and dev tools
      ocaml
      findlib
      dune
      utop
      ncurses
      merlin
      ocamlformat
      nodePackages.graphql-cli
      # Direct dependencies
      base
      cohttp
      cohttp-lwt-unix
      hex
      nocrypto
      yojson
      # Dependencies of vendored dependencies
      menhir
      ocaml-migrate-parsetree
      ppx_metaquot
      ppx_tools_versioned
      reason
      result
      rresult
      # Publishing
      heroku
    ];

  shellHook = "export OCAMLFORMAT_LOCATION=${ocamlformat}";
}
