{ lib, pkgs }:
pkgs.writeShellScriptBin "gcp-iap-proxy" ''
  set -euo pipefail
  exec 2>/dev/null
  host="$1"
  port="$2"

  case "$host" in
    gcp1-*) project="$GCP_DELIVERY_DEV" ;;
    pg1-*)  project="$GCP_DELIVERY_PROD" ;;
    *)      project="" ;;
  esac

  cache_dir="''${XDG_CACHE_HOME:-$HOME/.cache}/gcp-zones"
  mkdir -p "$cache_dir"
  cache_file="$cache_dir/$host"

  if [[ -f "$cache_file" ]]; then
    zone=$(<"$cache_file")
  else
    # When it doesn't have a tty, start-iap-tunnel needs --zone to be
    # specified. The following extracts that, based on the gcloud
    # completion code
    zone=$(IFS=$'\013' \
      COMP_LINE="gcloud compute ssh $host" \
      COMP_POINT=''${#COMP_LINE} \
      _ARGCOMPLETE_COMP_WORDBREAKS="" \
      _ARGCOMPLETE=1 \
      ${lib.getExe pkgs.google-cloud-sdk} 8>&1 9>&2 1>/dev/null | grep -oP '(?<=--zone=)[^ \\]+')
    [[ -n "$zone" ]] && echo "$zone" > "$cache_file"
  fi

  exec ${lib.getExe pkgs.google-cloud-sdk} compute start-iap-tunnel "$host" "$port" \
    ''${project:+--project="$project"} \
    ''${zone:+--zone="$zone"} \
    --listen-on-stdin \
    --verbosity=warning
''
