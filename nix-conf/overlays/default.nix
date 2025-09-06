{ inputs, ... }:
let
  files = builtins.filter (f: f != "default.nix") (builtins.attrNames (builtins.readDir ./.));
in
# Import every overlays/foo.nix file as overlays.foo (except for this file)
builtins.listToAttrs (
  map (file: {
    name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
    value = import (./. + "/${file}") { inherit inputs; };
  }) files
)
