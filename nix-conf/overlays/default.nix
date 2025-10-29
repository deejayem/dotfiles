{ inputs, ... }:
let
  files = builtins.filter (
    f: f != "default.nix" && f != "flake-module.nix" && builtins.match ".*\\.nix$" f != null
  ) (builtins.attrNames (builtins.readDir ./.));
in
# Import (almost) every overlays/foo.nix file as overlays.foo
builtins.listToAttrs (
  map (file: {
    name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
    value = import (./. + "/${file}") { inherit inputs; };
  }) files
)
