{ pkgs ? import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/5ea573ca476915e0ccefb99be5687e4150fa049b.tar.gz";
  sha256 = "0hsg6xjj4iclfrvplsf0rd9xiwkaazvmlp609qiwka65v7wcjja2";
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
