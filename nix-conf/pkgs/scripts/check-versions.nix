{ lib, pkgs }:

let
  column = lib.getExe' pkgs.util-linux "column";
  curl = lib.getExe pkgs.curl;
  grep = lib.getExe pkgs.gnugrep;
  jq = lib.getExe pkgs.jq;
  nix = lib.getExe pkgs.nix;
  sort = lib.getExe' pkgs.coreutils "sort";
  tail = lib.getExe' pkgs.coreutils "tail";
in
pkgs.writeShellScriptBin "check-versions" ''
  set -euo pipefail

  flake_dir="''${1:-.}"

  green=$'\033[32m'
  red=$'\033[31m'
  bold=$'\033[1m'
  reset=$'\033[0m'

  nix_ver() {
    ${nix} eval --raw "$1" 2>/dev/null
  }

  yellow=$'\033[33m'

  color_ver() {
    local ver="$1" latest="$2" width="$3" color="$4"
    printf '%b%-*s%b' "$color" "$width" "$ver" "$reset"
  }

  latest_chrome=$(${curl} -s 'https://versionhistory.googleapis.com/v1/chrome/platforms/mac_arm64/channels/stable/versions/all/releases?filter=endtime=none,fraction%3E=0.5&order_by=version%20desc' | ${jq} -r '.releases[0].version')
  latest_firefox=$(${curl} -s https://archive.mozilla.org/pub/firefox/releases/ | ${grep} -oP '[0-9]+\.[0-9.]+(?=/)' | ${sort} -t. -k1,1n -k2,2n -k3,3n | ${tail} -1)
  latest_slack=$(${curl} -s 'https://slack.com/api/desktop.latestRelease?arch=arm64&variant=dmg' | ${jq} -r .version)
  latest_zoom=$(${curl} -Ls 'https://zoom.us/rest/download?os=mac' | ${jq} -r '.result.downloadVO.zoomArm64.version')

  nixpkgs_chrome=$(nix_ver nixpkgs#google-chrome.version)
  nixpkgs_firefox=$(nix_ver nixpkgs#firefox.version)
  nixpkgs_slack=$(nix_ver nixpkgs#slack.version)
  nixpkgs_zoom=$(nix_ver nixpkgs#zoom-us.version)

  local_chrome=$(nix_ver "''${flake_dir}#modifications.google-chrome.version")
  local_firefox=$(nix_ver "''${flake_dir}#modifications.firefox.version")
  local_slack=$(nix_ver "''${flake_dir}#modifications.slack.version")
  local_zoom=$(nix_ver "''${flake_dir}#modifications.zoom-us.version")

  w=18

  hline() {
    local left="$1" mid="$2" right="$3"
    printf '%s' "$left"
    printf '─%.0s' $(seq 1 $((w+2))); printf '%s' "$mid"
    printf '─%.0s' $(seq 1 $((w+2))); printf '%s' "$mid"
    printf '─%.0s' $(seq 1 $((w+2))); printf '%s' "$mid"
    printf '─%.0s' $(seq 1 $((w+2))); printf '%s\n' "$right"
  }

  header() {
    printf "│ ''${bold}%-''${w}s''${reset} │ ''${bold}%-''${w}s''${reset} │ ''${bold}%-''${w}s''${reset} │ ''${bold}%-''${w}s''${reset} │\n" PACKAGE NIXPKGS LOCAL LATEST
  }

  row() {
    local name="$1" nixpkgs="$2" local_v="$3" latest="$4"
    local nixpkgs_color local_color
    if [[ "$local_v" == "$latest" ]]; then
      local_color="$green"
      nixpkgs_color=$(if [[ "$nixpkgs" == "$latest" ]]; then echo "$green"; else echo "$yellow"; fi)
    else
      local_color="$red"
      nixpkgs_color=$(if [[ "$nixpkgs" == "$latest" ]]; then echo "$green"; else echo "$red"; fi)
    fi
    printf '│ %-*s │ ' "$w" "$name"
    color_ver "$nixpkgs" "$latest" "$w" "$nixpkgs_color"
    printf ' │ '
    color_ver "$local_v" "$latest" "$w" "$local_color"
    printf ' │ %-*s │\n' "$w" "$latest"
  }

  hline '┌' '┬' '┐'
  header
  hline '├' '┼' '┤'
  row chrome  "$nixpkgs_chrome"  "$local_chrome"  "$latest_chrome"
  row firefox "$nixpkgs_firefox" "$local_firefox" "$latest_firefox"
  row slack   "$nixpkgs_slack"   "$local_slack"   "$latest_slack"
  row zoom    "$nixpkgs_zoom"    "$local_zoom"    "$latest_zoom"
  hline '└' '┴' '┘'
''
