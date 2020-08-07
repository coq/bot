{ pkgs ? import (fetchTarball {
  name = "nixpkgs-master-2020-08-05";
  url =
    "https://github.com/NixOS/nixpkgs/archive/143626989245c751505049316ae408737248309d.tar.gz";
  sha256 = "0r2pizvwm7skr6zdwan355ffsb439knk6kb2m0z9xmps67lkjy18";
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
    toml
    # Publishing
    heroku
  ];

  shellHook = ''
    export OCAMLFORMAT_LOCATION=${ocamlformat}
  '';
}
