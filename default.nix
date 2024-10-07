{ pkgs ? import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/280db3decab4cbeb22a4599bd472229ab74d25e1.tar.gz";
  sha256 = "sha256:17n9wji64l7d16s8r100ypwlxkmwrypll4q3wkkfjswbilxkqjr6";
}) { } }:

pkgs.stdenv.mkDerivation rec {
  name = "coqbot";
  src = null;
  buildInputs = with pkgs.ocamlPackages; [ # Compiler and dev tools
    ocaml
    findlib
    dune_3
    utop
    pkgs.ncurses
    merlin
    ocaml-lsp
    pkgs.ocamlformat
    pkgs.nixfmt-rfc-style
    pkgs.nodePackages.get-graphql-schema
    # Direct dependencies
    base
    camlzip
    cohttp
    cohttp-lwt-unix
    ohex
    iso8601
    mirage-crypto
    mirage-crypto-rng-lwt
    yojson
    graphql_ppx
    toml
    eqaf
    x509
    digestif
    ppx_expect
    odoc
  ];

  shellHook = ''
    export OCAMLFORMAT_LOCATION=${pkgs.ocamlformat}
  '';
}
