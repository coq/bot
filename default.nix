{ pkgs ? import (fetchTarball {
  name = "nixpkgs-unstable-2020-12-21";
  url =
    "https://github.com/NixOS/nixpkgs/archive/b6f7f1f673046ad2851728cb8f3a2f0888a1dcf0.tar.gz";
  sha256 = "1f7w0kavvbh5rfvv04ja3hj5rzn1hp0ff0yq9wzbpzjchjskaf5w";
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
