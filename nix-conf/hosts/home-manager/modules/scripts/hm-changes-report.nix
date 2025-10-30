{ config, pkgs, ... }:

# https://github.com/gvolpe/nix-config/blob/e28a220d0087064e6bad6b992b4914a65eb545e5/home/scripts/changes-report.nix
let
  hm-profiles = "${config.home.homeDirectory}/.local/state/nix/profiles/home-manager-*-link";
in
pkgs.writeShellScriptBin "hm-changes-report" ''
  # Disable nvd if there are less than 2 hm profiles.
  if [ $(ls -d1v ${hm-profiles} 2>/dev/null | wc -l) -lt 2 ]; then
    echo "Skipping changes report..."
  else
    ${pkgs.nvd}/bin/nvd diff $(ls -d1v ${hm-profiles} | tail -2)
  fi
''
