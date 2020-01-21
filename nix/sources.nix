# nix-build -E \
#  'with import <nixpkgs> { overlays = [(import ./.)];}; pkgs.bs-platform'
{ pkgsPath ? <nixpkgs> }:

let
  # overlays = /home/anmonteiro/projects/nix-overlays;
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/de5f7fa.tar.gz;
    sha256 = "18rjgjp2raws8zz4n08k74hxfayshxq2zkxfjvz52f001qrbw5hi";
  };

  pkgs = import pkgsPath { overlays = [ (import overlays) ]; };

  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "7415c4f";
    sha256 = "1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  };

  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

in
  { inherit pkgs gitignoreSource; }

