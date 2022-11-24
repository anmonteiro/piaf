{ stdenv, lib, ocamlPackages, static ? false, doCheck, nix-filter }:

with ocamlPackages;

rec {
  piaf = buildDunePackage {
    pname = "piaf";
    version = "0.0.1-dev";

    src = with nix-filter; filter {
      root = ./..;
      include = [
        "dune"
        "dune-project"
        "piaf.opam"
        "dune-project"
      ] ++ (builtins.map inDirectory [
        "lib"
        "lib_test"
        "stream"
        "multipart"
        "multipart_test"
      ]);
    };

    useDune2 = true;

    nativeBuildInputs = [ ocaml dune findlib ];
    propagatedBuildInputs = [
      logs
      eio-ssl
      magic-mime
      ssl
      uri
      ipaddr

      httpaf-eio
      gluten-eio
      h2-eio
      websocketaf

      multipart_form

      dune-site

      # Not in checkInputs because we also run tests in the musl64 build
      alcotest
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

    src = with nix-filter; filter {
      root = ./..;
      include = [
        "dune"
        "dune-project"
        "bin"
      ];
    };

    nativeBuildInputs = [ dune ocaml findlib ];

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

    meta = {
      description = "`curl` clone implemented using Piaf.";
      license = lib.licenses.bsd3;
    };
  };
}
