{ lib, pkgs, ... }:

pkgs.writeShellScriptBin "home-diff-closures" ''
  home_profiles="$HOME/.local/state/nix/profiles/home-manager-*-link"

  # Skip the diff if there are fewer than 2 home profiles.
  if [ $(ls -d1v $home_profiles 2>/dev/null | wc -l) -lt 2 ]; then
    echo "Skipping closure diff..."
  else
    ${lib.getExe pkgs.nix} store diff-closures $(ls -d1v $home_profiles | tail -2)
  fi
''
