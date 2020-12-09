{ pkgs ? import (fetchTarball {
  name = "nixpkgs-pr-106386-2020-12-09";
  url =
    "https://github.com/NixOS/nixpkgs/archive/e0e4bd68f3b15ef77c277712bc652976abd1988b.tar.gz";
  sha256 = "1pdv3iszinq9z06symv515xnv3s4r2s6b681h8gva705s1gqfpav";
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
