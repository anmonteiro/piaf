{ ocamlVersion }:

let
  lock = builtins.fromJSON (builtins.readFile ./../../flake.lock);
  src = fetchGit {
    url = with lock.nodes.nixpkgs.locked;"https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nixpkgs.locked) rev;
  };
  pkgs = import src {
    extraOverlays = [
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";
      })
    ];
  };
  nix-filter-src = fetchGit {
    url = with lock.nodes.nix-filter.locked; "https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nix-filter.locked) rev;
    # inherit (lock.nodes.nixpkgs.original) ref;
    allRefs = true;
  };
  nix-filter = import "${nix-filter-src}";

  inherit (pkgs) lib stdenv fetchTarball ocamlPackages callPackage;
  native = callPackage ./.. {
    doCheck = false;
    inherit nix-filter;
  };
  musl =
    let
      pkgs' = pkgs.pkgsCross.musl64;
    in
    pkgs'.lib.callPackageWith pkgs' ./.. {
      inherit nix-filter;
      doCheck = false;
      static = true;
    };

  test = pkg:
    let
      piafDrvs = lib.filterAttrs
        (_: value:
          if lib.isDerivation value
          then true
          else false)
        pkg;
    in
    stdenv.mkDerivation {
      name = "piaf-tests";
      src = with nix-filter; filter {
        root = ./../..;
        include = [
          ".ocamlformat"
          ".ocamlformat-ignore"
          "piaf.opam"
          "piaf-lwt.opam"
          "dune-project"
          "dune"
        ] ++ (builtins.map inDirectory [
          "lib"
          "lib_test"
          "multipart"
          "multipart_test"
          "lib-lwt"
          "lib-lwt_test"
          "multipart-lwt"
          "multipart-lwt_test"
          "stream"
          "examples"
        ]);
      };

      buildPhase = ''
        # Run these 2 tests serially as they use the same server ports
        dune runtest -p piaf-lwt
        ${if (lib.hasPrefix "5_" ocamlVersion) then
          "dune runtest -p piaf"
          else ""}
      '';
      installPhase = ''
        touch $out
      '';
      buildInputs = (lib.attrValues piafDrvs) ++ (with ocamlPackages; [
        ocaml
        dune
        findlib
        ocamlformat
      ]);
      doCheck = true;
      checkPhase = ''
        # Check code is formatted with OCamlformat
        dune build @fmt
      '';
    };
in
{
  native = test native;
  musl64 = test musl;
}
