{ inputs, ... }:
let
  # Sort the overlays, to make extra sure things are consistent
  files = builtins.sort (a: b: a < b) (
    builtins.filter (f: f != "default.nix" && builtins.match ".*\\.nix$" f != null) (
      builtins.attrNames (builtins.readDir ./.)
    )
  );
in
# Import every overlays/foo.nix file as overlays.foo (except for this file)
builtins.listToAttrs (
  map (file: {
    name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
    value = import (./. + "/${file}") { inherit inputs; };
  }) files
)
