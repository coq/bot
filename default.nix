{ pkgs ? import (fetchTarball {
  name = "nixpkgs-unstable-2021-08-28";
  url =
    "https://github.com/NixOS/nixpkgs/archive/08ef0f28e3a41424b92ba1d203de64257a9fca6a.tar.gz";
  sha256 = "1mql1gp86bk6pfsrp0lcww6hw5civi6f8542d4nh356506jdxmcy";
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
    iso8601
    mirage-crypto
    yojson
    graphql_ppx
    toml
    eqaf
    x509
    cstruct
    odoc
    # Publishing
    heroku
  ];

  shellHook = ''
    export OCAMLFORMAT_LOCATION=${ocamlformat}
  '';
}
