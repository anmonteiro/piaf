{ pkgs, stdenv, ocamlPackages }:

  ocamlPackages.buildDunePackage ({
    pname = "piaf";
    version = "0.0.1-dev";

    src = ./.;

    propagatedBuildInputs = with ocamlPackages; [
      ocaml
      merlin
      dune
      httpaf
      httpaf-lwt-unix
      findlib
      h2
      h2-lwt-unix
      logs
      lwt_ssl
      ssl
      pkgs.openssl.dev
      uri
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
  })
