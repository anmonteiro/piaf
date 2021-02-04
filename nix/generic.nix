{ stdenv, lib, ocamlPackages, static ? false, doCheck }:

with ocamlPackages;

rec {
  piaf = buildDunePackage {
    pname = "piaf";
    version = "0.0.1-dev";

    src = lib.filterGitSource {
      src = ./..;
      dirs = [ "lib" "lib_test" "multipart" "multipart_test" "vendor" ];
      files = [ "dune" "dune-project" "piaf.opam" ];
    };

    useDune2 = true;

    nativeBuildInputs = [ ocaml dune findlib ];
    propagatedBuildInputs = [
      logs
      lwt_ssl
      magic-mime
      ssl
      uri

      # (vendored) httpaf dependencies
      angstrom
      faraday
      gluten-lwt-unix

      # (vendored) h2 dependencies
      psq

      # (vendored) multipart_form dependencies
      pecu
      mrmime

      alcotest
      alcotest-lwt
    ];

    inherit doCheck;

    meta = {
      description = "Client library for HTTP/1.X / HTTP/2 written entirely in OCaml.";
      license = lib.licenses.bsd3;
    };
  };

  carl = stdenv.mkDerivation {
    name = "carl";
    version = "0.0.1-dev";

    src = lib.filterGitSource {
      src = ./..;
      dirs = [ "bin" ];
      files = [ "dune" "dune-project" ];
    };

    nativeBuildInputs = [dune ocaml findlib];

    buildPhase = ''
      echo "running ${if static then "static" else "release"} build"
      dune build bin/carl.exe --display=short --profile=${if static then "static" else "release"}
    '';
    installPhase = ''
      mkdir -p $out/bin
      mv _build/default/bin/carl.exe $out/bin/carl
    '';

    buildInputs = [
      piaf
      cmdliner
      fmt
      camlzip
      ezgzip
    ];

    inherit doCheck;

    meta = {
      description = "`curl` clone implemented using Piaf.";
      license = lib.licenses.bsd3;
    };
  };
}

