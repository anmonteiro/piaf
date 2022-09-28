{ lib }:

let
  commonSteps = [
    {
      uses = "actions/checkout@v2";
      "with" = {
        "submodules" = "recursive";
      };
    }
    {
      uses = "cachix/install-nix-action@v17";
      "with" = {
        extra_nix_config = ''
          extra-substituters = https://anmonteiro.nix-cache.workers.dev
          extra-trusted-public-keys = ocaml.nix-cache.com-1:/xI2h2+56rwFfKyyFVbkJSeGqSIYMC/Je+7XXqGKDIY=
        '';
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
    cachedBuild = { name, branches ? [ "master" ], os }:
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
              steps = commonSteps ++ [{ inherit name run; }];
            } // (if (conf ? ocamlVersions) then {
              inherit (conf) ocamlVersions;
            } else { })))
          os;
      };
  };

in

gh-actions.cachedBuild {
  name = "Build";
  os = {
    macos-latest = {
      name = "Run nix-build";
      ocamlVersions = [ "4_14" "5_00" ];
      run = "nix-build ./nix/ci/test.nix -A native --argstr ocamlVersion \${{ matrix.ocamlVersion }}";
    };
    ubuntu-latest = {
      ocamlVersions = [ "4_13" "4_14" "5_00" ];
      name = "Run nix-build";
      run = "nix-build ./nix/ci/test.nix -A native -A musl64 --argstr ocamlVersion \${{ matrix.ocamlVersion }}";
    };
  };

}
