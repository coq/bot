{ pkgs ? import (fetchTarball {
  name = "pr-93179-2020-07-15";
  url =
    "https://github.com/NixOS/nixpkgs/archive/49496772d214aff4f361f90fc95d071bd04df890.tar.gz";
  sha256 = "0gr6a75s3z6hga7bpz3zwfmf7dc12rhv07rw72qyysk86wl1rf6y";
}) { } }:

with pkgs;
let ocamlPackages = ocaml-ng.ocamlPackages_4_10;
in stdenv.mkDerivation rec {
  name = "coqbot";
  src = null;
  buildInputs = with ocamlPackages; [ # Compiler and dev tools
    ocaml
    findlib
    dune_2
    utop
    ncurses
    merlin
    ocamlformat
    nixfmt
    nodePackages.get-graphql-schema
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
