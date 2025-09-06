{ inputs, ... }:
{
  additions = import ./additions.nix { inherit inputs; };
  unstable-packages = import ./unstable-packages.nix { inherit inputs; };
  lazy-flakes = import ./lazy-flakes.nix { inherit inputs; };
}
