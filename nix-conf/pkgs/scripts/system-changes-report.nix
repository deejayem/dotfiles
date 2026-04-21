{ pkgs, ... }:

# https://github.com/gvolpe/nix-config/blob/e28a220d0087064e6bad6b992b4914a65eb545e5/home/scripts/changes-report.nix
let
  system-profiles = "/nix/var/nix/profiles/system-*-link";
in
pkgs.writeShellScriptBin "system-changes-report" ''
  # Skip the diff if there are less than 2 system profiles.
  if [ $(ls -d1v ${system-profiles} 2>/dev/null | wc -l) -lt 2 ]; then
    echo "Skipping changes report..."
  else
    ${pkgs.dix}/bin/dix $(ls -d1v ${system-profiles} | tail -2)
  fi
''
