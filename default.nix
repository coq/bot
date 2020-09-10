{ pkgs ? import (fetchTarball {
  name = "nixpkgs-master-2020-08-08";
  url =
    "https://github.com/NixOS/nixpkgs/archive/1074c0a37f2bb032767d24620123d7817eead9d0.tar.gz";
  sha256 = "1q64cxf0lbm0fzyw6sxi3bzv8bkp4j4v0pp9n6jhq5wwh8b9bm8j";
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
