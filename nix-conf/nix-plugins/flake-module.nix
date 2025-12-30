{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.nix-plugins = import ./package.nix { inherit pkgs; };
    };
}
