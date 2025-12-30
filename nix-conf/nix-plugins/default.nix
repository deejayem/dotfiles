{ pkgs, ... }:
let
  nix-plugins = import ./package.nix { inherit pkgs; };
in
{
  nix.settings = {
    plugin-files = "${nix-plugins}/lib/nix/plugins";
    extra-builtins-file = [ ./extra-builtins.nix ];
  };
}
