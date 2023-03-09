{ pkgs ? import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/92d2950194214e975ae7bd2655036689fccac155.tar.gz";
  sha256 = "1x8nk008bz5vkmqm7syxm68b0zchias039ppqjjz7rx58cqpikrk";
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
    pkgs.nixfmt
    pkgs.nodePackages.get-graphql-schema
    # Direct dependencies
    base
    cohttp
    cohttp-lwt-unix
    hex
    iso8601
    mirage-crypto
    mirage-crypto-rng-lwt
    yojson
    graphql_ppx
    toml
    eqaf
    x509
    cstruct
    ppx_expect
    odoc
  ];

  shellHook = ''
    export OCAMLFORMAT_LOCATION=${pkgs.ocamlformat}
  '';
}
