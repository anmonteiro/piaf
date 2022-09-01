{ lib }:

let
  commonSteps =
    { signingKey
    , endpoint ? "s3://overlays?endpoint=https://7a53c28e9b7a91239f9ed42da04276bc.r2.cloudflarestorage.com"
    , awsAccessKeyId
    , awsSecretAccessKey
    }: [
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
        uses = "ulrikstrid/nix-s3-action@fork";
        "with" = {
          inherit endpoint signingKey awsAccessKeyId awsSecretAccessKey;
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
    cachedBuild = { name, branches ? [ "master" ], os, cache }:
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
              steps = commonSteps cache
                ++ [{ inherit name run; }];
            } // (if (conf ? ocamlVersions) then {
              inherit (conf) ocamlVersions;
            } else { })))
          os;
      };
  };

in

gh-actions.cachedBuild {
  name = "Build";
  cache = {
    signingKey = "\${{ secrets.NIX_CACHE_SIGNING_KEY }}";
    awsAccessKeyId = "\${{ secrets.NIX_CACHE_KEY_ID }}";
    awsSecretAccessKey = "\${{ secrets.NIX_CACHE_SECRET_KEY }}";
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
