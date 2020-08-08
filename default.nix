{ pkgs ? import (fetchTarball {
  name = "nixpkgs-master-2020-08-08";
  url =
    "https://github.com/NixOS/nixpkgs/archive/b10e79a2616502688e2a9ccea1a7ca22d1f60f54.tar.gz";
  sha256 = "13xm731qalpx8qpxr85kx2abxv6lizws8izxcncwd7x3rv4hc1y0";
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
