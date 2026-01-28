{ lib, pkgs }:

pkgs.writeShellScriptBin "gcp-iap-proxy" ''
  set -euo pipefail

  host="$1" # needs %n in ProxyCommand (not %h)
  port="$2"

  IFS='.' read -r instance zone project <<< "$host"

  exec ${lib.getExe pkgs.google-cloud-sdk} compute start-iap-tunnel "$instance" "$port" \
    --zone="$zone" \
    --project="$project" \
    --listen-on-stdin \
    --verbosity=warning
''
