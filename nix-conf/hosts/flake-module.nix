{ inputs, self, ... }:
let
  definitions = import ./definitions.nix { inherit inputs; };
  builders = import ./builders.nix { inherit inputs self; };
  inherit (definitions) versions hosts;
in
{
  flake = builders.mkConfigurations versions hosts;
}
