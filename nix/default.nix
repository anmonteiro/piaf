{ stdenv, darwin, lib, ocamlPackages, static ? false, doCheck, nix-filter }:

rec {
  piaf = ocamlPackages.buildDunePackage {
    pname = "piaf";
    version = "0.0.1-dev";

    src = with nix-filter; filter {
      root = ./..;
      include = [
        "dune"
        "dune-project"
        "piaf.opam"
        "dune-project"
        "lib"
        "lib_test"
        "sendfile"
        "stream"
        "multipart"
        "multipart_test"
      ];
    };

    propagatedBuildInputs = with ocamlPackages; [
      logs
      eio-ssl
      magic-mime
      ssl
      uri
      ipaddr

      httpun-eio
      gluten-eio
      h2-eio
      httpun-ws

      multipart_form

      dune-site

      # Not in checkInputs because we also run tests in the musl64 build
      alcotest
    ] ++ lib.optionals (stdenv.isDarwin && !stdenv.isAarch64)
      (with darwin.apple_sdk_11_0; [
        Libsystem
      ]);
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

    nativeBuildInputs = with ocamlPackages; [ dune ocaml findlib ];

    buildPhase = ''
      echo "running ${if static then "static" else "release"} build"
      dune build bin/carl.exe --display=short --profile=${if static then "static" else "release"}
    '';
    installPhase = ''
      mkdir -p $out/bin
      mv _build/default/bin/carl.exe $out/bin/carl
    '';

    buildInputs = with ocamlPackages; [
      cmdliner
      fmt
      camlzip
      ezgzip
    ] ++ [ piaf ];

    meta = {
      description = "`curl` clone implemented using Piaf.";
      license = lib.licenses.bsd3;
    };
  };
}
