{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.nix-plugins = import ./nix-plugins.nix { inherit pkgs; };
    };
}
