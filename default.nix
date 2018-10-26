{ pkgs ?
  import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/cea4806bc5c425d2f9d9e110ae425b010e870662.tar.gz";
    sha256 = "0i0n2c5w585n04m5hqdcqrygdcpb46ncn0bchwp6mhjyd34zyckb";
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
      # Libraries
      base
      cohttp
      cohttp-lwt-unix
      yojson
      # Publishing
      heroku
    ];

  shellHook = "export OCAMLFORMAT_LOCATION=${ocamlformat}";
}
