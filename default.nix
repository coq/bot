{ pkgs ? import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/d003639e6f1266a0847df3bf1703dde4744c03f0.tar.gz";
  sha256 = "1w0i46rv5f03f17v3x5m4fsnbw3811h8xkw7hf2nfrxv0nzgfb7y";
}) { } }:

pkgs.stdenv.mkDerivation rec {
  name = "coqbot";
  src = null;
  buildInputs = with pkgs.ocamlPackages; [ # Compiler and dev tools
    ocaml
    findlib
    dune_2
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
    yojson
    graphql_ppx
    toml
    eqaf
    x509
    cstruct
    odoc
  ];

  shellHook = ''
    export OCAMLFORMAT_LOCATION=${pkgs.ocamlformat}
  '';
}
