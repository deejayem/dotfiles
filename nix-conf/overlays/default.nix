{ inputs, ... }:
let
  ignoreList = [
    "default\\.nix"
    "flake-module\\.nix"
    "_.*"
  ];

  files = builtins.filter (
    f:
    !(builtins.any (pattern: builtins.match pattern f != null) ignoreList)
    && builtins.match ".*\\.nix$" f != null
  ) (builtins.attrNames (builtins.readDir ./.));
in
# Import (almost) every overlays/foo.nix file as overlays.foo
builtins.listToAttrs (
  map (file: {
    name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
    value = import (./. + "/${file}") { inherit inputs; };
  }) files
)
