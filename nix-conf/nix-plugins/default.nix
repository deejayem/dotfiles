{ pkgs, ... }:
let
  nix-plugins = import ./package.nix { inherit pkgs; };
in
{
  nix.settings = {
    plugin-files = "${nix-plugins}/lib/nix/plugins";
    # Try to workround issue with non-existent /nix/store/<HASH>-source/nix-conf/nix-plugins/extra-builtins.nix
    # paths ending up in /etc/nix/nix.conf
    extra-builtins-file = "${nix-plugins}/share/nix-plugins/extra-builtins.nix";
  };
}
