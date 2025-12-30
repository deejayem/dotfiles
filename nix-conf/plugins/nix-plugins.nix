{ pkgs }:
pkgs.nix-plugins.override {
  nixComponents = pkgs.nixVersions.nixComponents_2_31;
}
