{ pkgs }:

let
  inherit (pkgs) stdenv;
  overlays =
    builtins.fetchTarball https://github.com/anmonteiro/nix-overlays/archive/master.tar.gz;

  ocamlPackages = pkgs.callPackage "${overlays}/ocaml.nix" {
    ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_09;
  };
in
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
