{ pkgs, stdenv, ocamlPackages, static ? false }:

rec {
  piaf = ocamlPackages.buildDune2Package {
    pname = "piaf";
    version = "0.0.1-dev";

    src = pkgs.lib.cleanSource ./..;
    nativeBuildInputs = with ocamlPackages; [dune_2];
    propagatedBuildInputs = with ocamlPackages; [
      bigstringaf
      findlib
      httpaf
      httpaf-lwt-unix
      h2
      h2-lwt-unix
      logs
      lwt_ssl
      ssl
      uri
    ];

    doCheck = false;

    meta = {
      description = "Client library for HTTP/1.X / HTTP/2 written entirely in OCaml.";
      license = stdenv.lib.licenses.bsd3;
    };
  };

  carl = stdenv.mkDerivation {
    name = "carl";
    version = "0.0.1-dev";

    src = pkgs.lib.cleanSource ./..;

    nativeBuildInputs = with ocamlPackages; [dune ocaml findlib];

    buildPhase = ''
      dune build bin/carl.exe --display=short --profile=${if static then "static" else "release"}
    '';
    installPhase = ''
      mkdir -p $out/bin
      mv _build/default/bin/carl.exe $out/bin/carl
    '';

    buildInputs = with ocamlPackages; [
      findlib
      piaf
      cmdliner
      fmt
      camlzip
      ezgzip
    ];

    doCheck = false;

    meta = {
      description = "Client library for HTTP/1.X / HTTP/2 written entirely in OCaml.";
      license = stdenv.lib.licenses.bsd3;
    };
  };
}

