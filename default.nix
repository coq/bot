{ pkgs ?
  import (fetchTarball {
    url = "https://github.com/Zimmi48/nixpkgs/archive/abfb32ef22c96c19dd992e75570d4fb50d2ab461.tar.gz";
    sha256 = "0prl42qpy8wzvd22pljb7mlyvc9k1sxdw2vkfky31v4xj9pkmxj4";
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
      # Libraries
      base
      cohttp
      cohttp-lwt-unix
      ppx_graphql
      yojson
      # Publishing
      heroku
    ];

  shellHook = "export OCAMLFORMAT_LOCATION=${ocamlformat}";
}
