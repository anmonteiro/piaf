{ lib }:

let
  commonSteps = { name, signingKey }: [
    {
      uses = "actions/checkout@v2";
      "with" = {
        "submodules" = "recursive";
      };
    }
    {
      uses = "cachix/install-nix-action@v17";
    }
    {
      uses = "cachix/cachix-action@v10";
      "with" = {
        inherit name signingKey;
      };
    }

  ];

  job =
    { steps
    , ocamlVersions ? [
        "4_13"
        "4_14"
        "5_00"
      ]
    , ...
    }@attrs: (builtins.removeAttrs attrs [ "ocamlVersions" ]) // {
      strategy = {
        fail-fast = false;
        matrix = {
          ocamlVersion = ocamlVersions
          ;
        };
      };
    };

  gh-actions = {
    cachixBuild = { name, branches ? [ "master" ], os, cachix }:
      lib.generators.toYAML { } {
        inherit name;
        on = {
          pull_request = null;
          push = {
            inherit branches;
          };
        };

        jobs = lib.mapAttrs
          (os: { run, name, ... }@conf:
            job ({
              runs-on = os;
              steps = commonSteps cachix
                ++ [{ inherit name run; }];
            } // (if (conf ? ocamlVersions) then {
              inherit (conf) ocamlVersions;
            } else { })))
          os;
      };
  };

in

gh-actions.cachixBuild {
  name = "Build";
  cachix = {
    name = "anmonteiro";
    signingKey = "\${{ secrets.CACHIX_SIGNING_KEY }}";
  };
  os = {
    macos-latest = {
      name = "Run nix-build";
      ocamlVersions = [ "4_13" "4_14" "5_00" ];
      run = "nix-build ./nix/ci/test.nix -A native --argstr ocamlVersion \${{ matrix.ocamlVersion }}";
    };
    ubuntu-latest = {
      ocamlVersions = [ "4_13" "4_14" "5_00" ];
      name = "Run nix-build";
      run = "nix-build ./nix/ci/test.nix -A native -A musl64 --argstr ocamlVersion \${{ matrix.ocamlVersion }}";
    };
  };

}
