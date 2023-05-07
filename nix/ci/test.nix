{ ocamlVersion }:

let
  lock = builtins.fromJSON (builtins.readFile ./../../flake.lock);
  src = fetchGit {
    url = with lock.nodes.nixpkgs.locked;"https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nixpkgs.locked) rev;
    allRefs = true;
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
      piafDrvsSet = lib.mapAttrs
        (_: value:
          if lib.isDerivation value
          then
            (if value ? "propagatedBuildInputs"
            then (lib.filter (p: (p.pname or "") != "piaf") value.propagatedBuildInputs)
            else [ ])
          else [ ])
        pkg;
      piafDrvs = lib.flatten (lib.attrValues piafDrvsSet);
    in
    stdenv.mkDerivation {
      name = "piaf-tests";
      src = with nix-filter; filter {
        root = ./../..;
        include = [
          ".ocamlformat"
          ".ocamlformat-ignore"
          "piaf.opam"
          "dune-project"
          "dune"
          "lib"
          "lib_test"
          "multipart"
          "multipart_test"
          "sendfile"
          "stream"
          "examples"
        ];
      };
      dontDetectOcamlConflicts = true;

      buildPhase = ''
        dune runtest -p piaf
      '';

      installPhase = ''
        touch $out
      '';
      buildInputs = piafDrvs ++ (with ocamlPackages; [
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
