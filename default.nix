{ pkgs ? import (fetchTarball {
  name = "nixos-unstable-2020-07-08";
  url =
    "https://github.com/NixOS/nixpkgs/archive/c3fb5d86a68bf606c2408629c1a1719eff08b06d.tar.gz";
  sha256 = "1i8d37h1yr7fxijx0yi0nj93g23a7kc0i6p5ckd6ccwzwbllr3y3";
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
    nodePackages.graphql-cli
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
