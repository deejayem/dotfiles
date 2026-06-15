{ lib, pkgs, ... }:

let
  system-profiles = "/nix/var/nix/profiles/system-*-link";
in
pkgs.writeShellScriptBin "system-diff-closures" ''
  # Skip the diff if there are less than 2 system profiles.
  if [ $(ls -d1v ${system-profiles} 2>/dev/null | wc -l) -lt 2 ]; then
    echo "Skipping closure diff..."
  else
    ${lib.getExe pkgs.nix} store diff-closures $(ls -d1v ${system-profiles} | tail -2)
  fi
''
