with import <nixpkgs> {};

let
  ocamlVersion = (builtins.parseDrvName ocamlPackages.ocaml.name).version;
  merlinWithEmacsMode = ocamlPackages.merlin.override { withEmacsMode = true; };
  findlibSiteLib = "${ocamlPackages.findlib}/lib/ocaml/${ocamlVersion}/site-lib";
in
stdenv.mkDerivation rec {
  name = "coqbot";
  src = null;
  buildInputs = with ocamlPackages;
    [ ocaml
      ocamlbuild
      findlib
      merlinWithEmacsMode
      cohttp cohttp-lwt-unix yojson
    ];
  # For my Emacs config...
  MERLIN_SITE_LISP = "${merlinWithEmacsMode}/share/emacs/site-lisp";
}
