{ inputs, ... }:
{
  additions = final: _prev: import ../pkgs final.pkgs;

  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
    };
  };

  mcp-nixos = final: _prev: {
    mcp-nixos =
      let
        upstream = builtins.getFlake "github:utensils/mcp-nixos/46b4d4d3d6421bfbadc415532ef74433871e1cda";
      in
        upstream.packages.${final.system}.default;
  };
}
