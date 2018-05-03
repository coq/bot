with import <nixpkgs> {};

let
  ocamlVersion = (builtins.parseDrvName ocamlPackages.ocaml.name).version;
  findlibSiteLib = "${ocamlPackages.findlib}/lib/ocaml/${ocamlVersion}/site-lib";
in
stdenv.mkDerivation rec {
  name = "coqbot";
  src = null;
  buildInputs = with ocamlPackages;
    [ ocaml
      findlib
      merlin
      cohttp
      cohttp-lwt-unix
      yojson
    ];
}
