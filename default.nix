{ pkgs ?
  import (fetchTarball {
    url = "https://github.com/Zimmi48/nixpkgs/archive/c40636e15dd690d25aec998b260e812e29227818.tar.gz";
    sha256 = "12cy25cg4mzxqjrmkzqp786pyq2gskl7r2d42vb1ddwq4rcgidq9";
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
