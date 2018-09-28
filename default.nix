{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation rec {
  name = "coqbot";
  src = null;
  buildInputs = with ocamlPackages;
    [ ocaml
      dune
      findlib
      merlin
      base
      cohttp
      cohttp-lwt-unix
      yojson
      heroku
    ];
}
