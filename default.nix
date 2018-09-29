{ pkgs ? import <nixpkgs> {} }:

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
      # Libraries
      base
      cohttp
      cohttp-lwt-unix
      yojson
      # Publishing
      heroku
    ];
}
