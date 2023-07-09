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
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope' (oself: osuper: {
          gluten = osuper.gluten.overrideAttrs (_: {
            src = super.fetchFromGitHub {
              owner = "anmonteiro";
              repo = "gluten";
              rev = "5d0d3134c69108a4eda0a86ca39ddfe8fa69bf3e";
              hash = "sha256-BPSLfgnO0uvLozrKbI2wGmTYgTMyY0YAaX5RYXw+f40=";
            };
          });

          hpack = osuper.hpack.overrideAttrs (_: {
            src = super.fetchFromGitHub {
              owner = "anmonteiro";
              repo = "ocaml-h2";
              rev = "4c97333c4c0f1dbce698da7d5f50217b43aadd8b";
              hash = "sha256-aWQwavWRhXly2Ic+blaaBkydM0CiU7I3nenxhfD4/7k=";
            };
          });

          eio-ssl = osuper.eio-ssl.overrideAttrs (_: {
            src = super.fetchFromGitHub {
              owner = "anmonteiro";
              repo = "eio-ssl";
              rev = "4492a4acce6720f9deec49448b01931d862a68e4";
              hash = "sha256-8iMwzqmql4JUMLrp5dB1TAg6y2Irzr4GGlvESaFNm/o=";
            };
          });

          httpaf = osuper.httpaf.overrideAttrs (_: {
            src = super.fetchFromGitHub {
              owner = "anmonteiro";
              repo = "httpaf";
              rev = "6c1f68fb56662e46ec94a16b1b42e3e92252359e";
              hash = "sha256-WoCm6QNQO/yxZhNf+TUaAoaQ+qUmEEvYfnOMooG1VkI=";
            };
          });
        });
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
