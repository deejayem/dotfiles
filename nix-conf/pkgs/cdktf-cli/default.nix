{ pkgs ? import <nixpkgs> {} }:

let
  nodePackages = import ./composition.nix {
    inherit pkgs;
    inherit (pkgs.stdenv.hostPlatform) system;
  };
in
nodePackages.cdktf-cli
