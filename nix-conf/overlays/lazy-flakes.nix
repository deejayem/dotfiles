{ inputs, ... }:

# Include flakes that we don't want to add to flake.nix, as they use too much disk space
# on systems where we don't install them

final: _prev: {
  mcp-nixos =
    let
      upstream = builtins.getFlake "github:utensils/mcp-nixos/46b4d4d3d6421bfbadc415532ef74433871e1cda";
    in
    upstream.packages.${final.system}.default;
}
