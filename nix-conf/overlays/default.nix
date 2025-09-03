{ inputs, ... }:
{
  local-packages = final: prev: import ../pkgs final.pkgs;

  remote-packages =
    final: prev:
    let
      system = final.system;
    in
    {
      mcp-nixos = inputs.mcp-nixos.packages.${system}.default;
    };

  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
    };
  };
}
