{ pkgs ? import (fetchTarball {
  name = "nixpkgs-unstable-2022-02-16";
  url =
    "https://github.com/NixOS/nixpkgs/archive/b66b39216b1fef2d8c33cc7a5c72d8da80b79970.tar.gz";
  sha256 = "0xqxrmdr3adcdj1ksnwf8w7d0qjzsc4sgzfcmvvrpk7hrc5q9cvg";
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
    ocaml-lsp
    ocamlformat
    nixfmt
    nodePackages.get-graphql-schema
    # Direct dependencies
    base
    cohttp
    cohttp-lwt-unix
    hex
    iso8601
    mirage-crypto
    yojson
    graphql_ppx
    toml
    eqaf
    x509
    cstruct
    odoc
  ];

  shellHook = ''
    export OCAMLFORMAT_LOCATION=${ocamlformat}
  '';
}
