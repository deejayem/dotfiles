{ lib, pkgs }:

let
  curl = lib.getExe pkgs.curl;
  grep = lib.getExe pkgs.gnugrep;
  jq = lib.getExe pkgs.jq;
  nix = lib.getExe pkgs.nix;
  sort = lib.getExe' pkgs.coreutils "sort";
  tail = lib.getExe' pkgs.coreutils "tail";
  uuidgen = lib.getExe' pkgs.util-linux "uuidgen";
  xmllint = lib.getExe' pkgs.libxml2 "xmllint";
in
pkgs.writeShellScriptBin "check-versions" ''
  set -uo pipefail

  flake_dir="''${1:-.}"

  green=$'\033[32m'
  yellow=$'\033[33m'
  red=$'\033[31m'
  bold=$'\033[1m'
  dim=$'\033[2m'
  reset=$'\033[0m'

  nix_ver() {
    ${nix} eval --raw "$1" 2>/dev/null || echo "error"
  }

  fetch_latest() {
    local result
    if result=$("$@" 2>/dev/null) && [[ -n "$result" && "$result" != "null" ]]; then
      echo "$result"
    else
      echo "error"
    fi
  }

  prefetch_sri() {
    ${nix} hash convert --hash-algo sha256 --to sri "$(${nix}-prefetch-url --type sha256 "$1" ''${2:+--name "$2"} 2>/dev/null)"
  }

  color_ver() {
    local ver="$1" latest="$2" width="$3" color="$4"
    printf '%b%-*s%b' "$color" "$width" "$ver" "$reset"
  }

  # Fetch latest versions
  latest_brave=$(fetch_latest sh -c "${curl} -s 'https://api.github.com/repos/brave/brave-browser/releases/latest' | ${jq} -r '.tag_name | ltrimstr(\"v\")'")
  latest_chrome=$(fetch_latest sh -c "${curl} -s 'https://versionhistory.googleapis.com/v1/chrome/platforms/mac_arm64/channels/stable/versions/all/releases?filter=endtime=none,fraction%3E=0.5&order_by=version%20desc' | ${jq} -r '.releases[0].version'")
  latest_firefox=$(fetch_latest sh -c "${curl} -s https://archive.mozilla.org/pub/firefox/releases/ | ${grep} -oP '[0-9]+\.[0-9.]+(?=/)' | ${sort} -t. -k1,1n -k2,2n -k3,3n | ${tail} -1")
  latest_slack=$(fetch_latest sh -c "${curl} -s 'https://slack.com/api/desktop.latestRelease?arch=arm64&variant=dmg' | ${jq} -r .version")
  latest_zoom=$(fetch_latest sh -c "${curl} -Ls 'https://zoom.us/rest/download?os=mac' | ${jq} -r '.result.downloadVO.zoomArm64.version'")

  # Fetch nixpkgs versions
  nixpkgs_brave=$(nix_ver nixpkgs#brave.version)
  nixpkgs_chrome=$(nix_ver nixpkgs#google-chrome.version)
  nixpkgs_firefox=$(nix_ver nixpkgs#firefox.version)
  nixpkgs_slack=$(nix_ver nixpkgs#slack.version)
  nixpkgs_zoom=$(nix_ver nixpkgs#zoom-us.version)

  # Fetch local versions
  local_brave=$(nix_ver "''${flake_dir}#modifications.brave.version")
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
    if [[ "$latest" == "error" ]]; then
      nixpkgs_color="$red"; local_color="$red"
    elif [[ "$local_v" == "$latest" ]]; then
      local_color="$green"
      nixpkgs_color=$(if [[ "$nixpkgs" == "$latest" ]]; then echo "$green"; else echo "$yellow"; fi)
    else
      local_color="$red"
      nixpkgs_color=$(if [[ "$nixpkgs" == "$latest" ]]; then echo "$green"; else echo "$red"; fi)
    fi
    [[ "$nixpkgs" == "error" ]] && nixpkgs_color="$red"
    [[ "$local_v" == "error" ]] && local_color="$red"
    printf '│ %-*s │ ' "$w" "$name"
    color_ver "$nixpkgs" "$latest" "$w" "$nixpkgs_color"
    printf ' │ '
    color_ver "$local_v" "$latest" "$w" "$local_color"
    printf ' │ %-*s │\n' "$w" "$latest"
  }

  hline '┌' '┬' '┐'
  header
  hline '├' '┼' '┤'
  row brave   "$nixpkgs_brave"   "$local_brave"   "$latest_brave"
  row chrome  "$nixpkgs_chrome"  "$local_chrome"  "$latest_chrome"
  row firefox "$nixpkgs_firefox" "$local_firefox" "$latest_firefox"
  row slack   "$nixpkgs_slack"   "$local_slack"   "$latest_slack"
  row zoom    "$nixpkgs_zoom"    "$local_zoom"    "$latest_zoom"
  hline '└' '┴' '┘'

  # Prefetch hashes for packages that need updating
  needs_update=false

  if [[ "$local_brave" != "$latest_brave" && "$latest_brave" != "error" ]]; then
    needs_update=true
    echo ""
    echo "''${bold}brave ''${latest_brave}:''${reset}"
    brave_url="https://github.com/brave/brave-browser/releases/download/v''${latest_brave}/brave-v''${latest_brave}-darwin-arm64.zip"
    echo "  ''${dim}url:  $brave_url''${reset}"
    echo "  ''${dim}hash: $(prefetch_sri "$brave_url")''${reset}"
  fi

  if [[ "$local_chrome" != "$latest_chrome" && "$latest_chrome" != "error" ]]; then
    needs_update=true
    echo ""
    echo "''${bold}chrome ''${latest_chrome}:''${reset}"
    uuid=$(${uuidgen})
    post_data="<?xml version='1.0' encoding='UTF-8'?><request protocol='3.0' version='1.3.23.9' shell_version='1.3.21.103' ismachine='1' sessionid='$uuid' installsource='ondemandcheckforupdate' requestid='$uuid' dedup='cr'><hw sse='1' sse2='1' sse3='1' ssse3='1' sse41='1' sse42='1' avx='1' physmemory='12582912' /><os platform='mac' version='$latest_chrome' arch='arm64'/><app appid='com.google.Chrome' ap=' ' version=' ' nextversion=' ' lang=' ' brand='GGLS' client=' '><updatecheck/></app></request>"
    response=$(${curl} -s -X POST -H "Content-Type: text/xml" --data "$post_data" "https://tools.google.com/service/update2")
    chrome_url="$(echo "$response" | ${xmllint} --xpath "string(//url[contains(@codebase, 'http://dl.google.com/release2')]/@codebase)" -)$(echo "$response" | ${xmllint} --xpath "string(//package/@name)" -)"
    echo "  ''${dim}url:  $chrome_url''${reset}"
    echo "  ''${dim}hash: $(prefetch_sri "$chrome_url")''${reset}"
  fi

  if [[ "$local_firefox" != "$latest_firefox" && "$latest_firefox" != "error" ]]; then
    needs_update=true
    echo ""
    echo "''${bold}firefox ''${latest_firefox}:''${reset}"
    firefox_url="https://archive.mozilla.org/pub/firefox/releases/''${latest_firefox}/source/firefox-''${latest_firefox}.source.tar.xz"
    echo "  ''${dim}url:   $firefox_url''${reset}"
    echo "  ''${dim}sha512: $(${nix}-prefetch-url --type sha512 "$firefox_url" 2>/dev/null)''${reset}"
  fi

  if [[ "$local_slack" != "$latest_slack" && "$latest_slack" != "error" ]]; then
    needs_update=true
    echo ""
    echo "''${bold}slack ''${latest_slack}:''${reset}"
    slack_url="https://downloads.slack-edge.com/desktop-releases/mac/arm64/''${latest_slack}/Slack-''${latest_slack}-macOS.dmg"
    echo "  ''${dim}url:  $slack_url''${reset}"
    echo "  ''${dim}hash: $(prefetch_sri "$slack_url")''${reset}"
  fi

  if [[ "$local_zoom" != "$latest_zoom" && "$latest_zoom" != "error" ]]; then
    needs_update=true
    echo ""
    echo "''${bold}zoom ''${latest_zoom}:''${reset}"
    zoom_url="https://zoom.us/client/''${latest_zoom}/zoomusInstallerFull.pkg?archType=arm64"
    echo "  ''${dim}url:  $zoom_url''${reset}"
    echo "  ''${dim}hash: $(prefetch_sri "$zoom_url" zoomusInstallerFull.pkg)''${reset}"
  fi

  if [[ "$needs_update" == "false" ]]; then
    echo ""
    echo "''${green}All packages up to date.''${reset}"
  fi
''
