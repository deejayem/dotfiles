{ lib, pkgs }:

let
  curl = lib.getExe pkgs.curl;
  grep = lib.getExe pkgs.gnugrep;
  jq = lib.getExe pkgs.jq;
  nix = lib.getExe pkgs.nix;
  sed = lib.getExe pkgs.gnused;
  seq = lib.getExe' pkgs.coreutils "seq";
  sort = lib.getExe' pkgs.coreutils "sort";
  tail = lib.getExe' pkgs.coreutils "tail";
  uuidgen = lib.getExe' pkgs.util-linux "uuidgen";
  xmllint = lib.getExe' pkgs.libxml2 "xmllint";
in
pkgs.writeShellScriptBin "check-versions" ''
  set -uo pipefail

  mode="check"
  allow_downgrade=false
  flake_dir=""

  for arg in "$@"; do
    case "$arg" in
      -u|--update) mode="update" ;;
      -i|--interactive) mode="interactive" ;;
      -f|--force) mode="force" ;;
      -d|--allow-downgrade) allow_downgrade=true ;;
      -*) echo "Usage: check-versions [-u|--update|-i|--interactive|-f|--force] [-d|--allow-downgrade] [flake-dir]" >&2; exit 1 ;;
      *) flake_dir="$arg" ;;
    esac
  done

  flake_dir="''${flake_dir:-.}"
  overrides_file="$flake_dir/overlays/_version-overrides.nix"

  green=$'\033[32m'
  yellow=$'\033[33m'
  red=$'\033[31m'
  blue=$'\033[34m'
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

  # Returns 0 (true) if $1 >= $2 (version comparison)
  version_ge() {
    [[ "$1" == "$2" ]] || [[ "$(printf '%s\n%s\n' "$1" "$2" | ${sort} -V | ${tail} -1)" == "$1" ]]
  }

  # Returns 0 (true) if a package should be updated, considering mode and downgrade policy
  should_update() {
    local local_v="$1" latest="$2"
    [[ "$latest" == "error" ]] && return 1
    if [[ "$local_v" == "$latest" ]]; then
      # Same version: only refresh in force mode
      [[ "$mode" == "force" ]]
    elif version_ge "$latest" "$local_v"; then
      return 0  # upgrade available
    else
      # Downgrade: only if explicitly allowed
      [[ "$allow_downgrade" == "true" ]]
    fi
  }

  latest_brave=$(fetch_latest sh -c "${curl} -s 'https://api.github.com/repos/brave/brave-browser/releases/latest' | ${jq} -r '.tag_name | ltrimstr(\"v\")'")
  latest_chrome=$(fetch_latest sh -c "${curl} -s 'https://versionhistory.googleapis.com/v1/chrome/platforms/mac_arm64/channels/stable/versions/all/releases?filter=endtime=none,fraction%3E=0.5&order_by=version%20desc' | ${jq} -r '.releases[0].version'")
  latest_firefox=$(fetch_latest sh -c "${curl} -s https://archive.mozilla.org/pub/firefox/releases/ | ${grep} -oP '[0-9]+\.[0-9.]+(?=/)' | ${sort} -t. -k1,1n -k2,2n -k3,3n | ${tail} -1")
  latest_orbstack=$(fetch_latest sh -c "${curl} -L -I -s 'https://orbstack.dev/download/stable/latest/arm64' | ${grep} -oE 'OrbStack_v[0-9._]+_arm64' | ${sed} 's/OrbStack_v//;s/_arm64//;s/_/-/'")
  latest_slack=$(fetch_latest sh -c "${curl} -s 'https://slack.com/api/desktop.latestRelease?arch=arm64&variant=dmg' | ${jq} -r .version")
  latest_zoom=$(fetch_latest sh -c "${curl} -Ls 'https://zoom.us/rest/download?os=mac' | ${jq} -r '.result.downloadVO.zoomArm64.version'")

  nixpkgs_brave=$(nix_ver nixpkgs#brave.version)
  nixpkgs_chrome=$(nix_ver nixpkgs#google-chrome.version)
  nixpkgs_firefox=$(nix_ver nixpkgs#firefox.version)
  nixpkgs_orbstack=$(nix_ver nixpkgs#orbstack.version)
  nixpkgs_slack=$(nix_ver nixpkgs#slack.version)
  nixpkgs_zoom=$(nix_ver nixpkgs#zoom-us.version)

  local_brave=$(nix_ver "''${flake_dir}#modifications.brave.version")
  local_chrome=$(nix_ver "''${flake_dir}#modifications.google-chrome.version")
  local_firefox=$(nix_ver "''${flake_dir}#modifications.firefox.version")
  local_orbstack=$(nix_ver "''${flake_dir}#modifications.orbstack.version")
  local_slack=$(nix_ver "''${flake_dir}#modifications.slack.version")
  local_zoom=$(nix_ver "''${flake_dir}#modifications.zoom-us.version")

  w=18

  hline() {
    local left="$1" mid="$2" right="$3"
    printf '%s' "$left"
    printf '─%.0s' $(${seq} 1 $((w+2))); printf '%s' "$mid"
    printf '─%.0s' $(${seq} 1 $((w+2))); printf '%s' "$mid"
    printf '─%.0s' $(${seq} 1 $((w+2))); printf '%s' "$mid"
    printf '─%.0s' $(${seq} 1 $((w+2))); printf '%s\n' "$right"
  }

  header() {
    printf "│ ''${bold}%-''${w}s''${reset} │ ''${bold}%-''${w}s''${reset} │ ''${bold}%-''${w}s''${reset} │ ''${bold}%-''${w}s''${reset} │\n" PACKAGE NIXPKGS LOCAL LATEST
  }

  row() {
    local name="$1" nixpkgs="$2" local_v="$3" latest="$4"
    local nixpkgs_color local_color
    if [[ "$latest" == "error" ]]; then
      nixpkgs_color="$red"; local_color="$red"
    else
      # Color for local
      if [[ "$local_v" == "error" ]]; then local_color="$red"
      elif [[ "$local_v" == "$latest" ]]; then local_color="$green"
      elif version_ge "$local_v" "$latest"; then local_color="$blue"
      else local_color="$red"
      fi
      # Color for nixpkgs
      if [[ "$nixpkgs" == "error" ]]; then nixpkgs_color="$red"
      elif [[ "$nixpkgs" == "$latest" ]]; then nixpkgs_color="$green"
      elif version_ge "$nixpkgs" "$latest"; then nixpkgs_color="$blue"
      elif version_ge "$local_v" "$latest"; then nixpkgs_color="$yellow"
      else nixpkgs_color="$red"
      fi
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
  row brave    "$nixpkgs_brave"    "$local_brave"    "$latest_brave"
  row chrome   "$nixpkgs_chrome"   "$local_chrome"   "$latest_chrome"
  row firefox  "$nixpkgs_firefox"  "$local_firefox"  "$latest_firefox"
  row orbstack "$nixpkgs_orbstack" "$local_orbstack" "$latest_orbstack"
  row slack    "$nixpkgs_slack"    "$local_slack"    "$latest_slack"
  row zoom     "$nixpkgs_zoom"     "$local_zoom"     "$latest_zoom"
  hline '└' '┴' '┘'

  declare -a outdated=()
  should_update "$local_brave"    "$latest_brave"    && outdated+=(brave)
  should_update "$local_chrome"   "$latest_chrome"   && outdated+=(chrome)
  should_update "$local_firefox"  "$latest_firefox"  && outdated+=(firefox)
  should_update "$local_orbstack" "$latest_orbstack" && outdated+=(orbstack)
  should_update "$local_slack"    "$latest_slack"    && outdated+=(slack)
  should_update "$local_zoom"     "$latest_zoom"     && outdated+=(zoom)

  if [[ ''${#outdated[@]} -eq 0 ]]; then
    echo ""
    echo "''${green}All packages up to date.''${reset}"
    exit 0
  fi

  if [[ "$mode" == "check" ]]; then
    for pkg in "''${outdated[@]}"; do
      case "$pkg" in
        brave)
          echo ""
          echo "''${bold}brave ''${latest_brave}:''${reset}"
          brave_url="https://github.com/brave/brave-browser/releases/download/v''${latest_brave}/brave-v''${latest_brave}-darwin-arm64.zip"
          echo "  ''${dim}url:  $brave_url''${reset}"
          echo "  ''${dim}hash: $(prefetch_sri "$brave_url")''${reset}"
          ;;
        chrome)
          echo ""
          echo "''${bold}chrome ''${latest_chrome}:''${reset}"
          uuid=$(${uuidgen})
          post_data="<?xml version='1.0' encoding='UTF-8'?><request protocol='3.0' version='1.3.23.9' shell_version='1.3.21.103' ismachine='1' sessionid='$uuid' installsource='ondemandcheckforupdate' requestid='$uuid' dedup='cr'><hw sse='1' sse2='1' sse3='1' ssse3='1' sse41='1' sse42='1' avx='1' physmemory='12582912' /><os platform='mac' version='$latest_chrome' arch='arm64'/><app appid='com.google.Chrome' ap=' ' version=' ' nextversion=' ' lang=' ' brand='GGLS' client=' '><updatecheck/></app></request>"
          response=$(${curl} -s -X POST -H "Content-Type: text/xml" --data "$post_data" "https://tools.google.com/service/update2")
          chrome_url="$(echo "$response" | ${xmllint} --xpath "string(//url[contains(@codebase, 'http://dl.google.com/release2')]/@codebase)" -)$(echo "$response" | ${xmllint} --xpath "string(//package/@name)" -)"
          echo "  ''${dim}url:  $chrome_url''${reset}"
          echo "  ''${dim}hash: $(prefetch_sri "$chrome_url")''${reset}"
          ;;
        firefox)
          echo ""
          echo "''${bold}firefox ''${latest_firefox}:''${reset}"
          firefox_url="https://archive.mozilla.org/pub/firefox/releases/''${latest_firefox}/source/firefox-''${latest_firefox}.source.tar.xz"
          echo "  ''${dim}url:   $firefox_url''${reset}"
          echo "  ''${dim}sha512: $(${nix}-prefetch-url --type sha512 "$firefox_url" 2>/dev/null)''${reset}"
          ;;
        orbstack)
          echo ""
          echo "''${bold}orbstack ''${latest_orbstack}:''${reset}"
          orbstack_url="https://cdn-updates.orbstack.dev/arm64/OrbStack_v$(echo "$latest_orbstack" | tr '-' '_')_arm64.dmg"
          echo "  ''${dim}url:  $orbstack_url''${reset}"
          echo "  ''${dim}hash: $(prefetch_sri "$orbstack_url")''${reset}"
          ;;
        slack)
          echo ""
          echo "''${bold}slack ''${latest_slack}:''${reset}"
          slack_url="https://downloads.slack-edge.com/desktop-releases/mac/arm64/''${latest_slack}/Slack-''${latest_slack}-macOS.dmg"
          echo "  ''${dim}url:  $slack_url''${reset}"
          echo "  ''${dim}hash: $(prefetch_sri "$slack_url")''${reset}"
          ;;
        zoom)
          echo ""
          echo "''${bold}zoom ''${latest_zoom}:''${reset}"
          zoom_url="https://zoom.us/client/''${latest_zoom}/zoomusInstallerFull.pkg?archType=arm64"
          echo "  ''${dim}url:  $zoom_url''${reset}"
          echo "  ''${dim}hash: $(prefetch_sri "$zoom_url" zoomusInstallerFull.pkg)''${reset}"
          ;;
      esac
    done
    exit 0
  fi

  change_summary() {
    local pkg="$1"
    local local_var="local_$pkg" latest_var="latest_$pkg"
    local local_v="''${!local_var}" latest="''${!latest_var}"
    if [[ "$local_v" == "$latest" ]]; then
      printf "%s: %s ''${dim}(refresh)''${reset}" "$pkg" "$local_v"
    elif version_ge "$latest" "$local_v"; then
      printf "%s: %s -> ''${green}%s''${reset}" "$pkg" "$local_v" "$latest"
    else
      printf "%s: %s -> ''${yellow}%s (downgrade)''${reset}" "$pkg" "$local_v" "$latest"
    fi
  }

  if [[ "$mode" == "interactive" ]]; then
    echo ""
    echo "The following changes will be applied:"
    for pkg in "''${outdated[@]}"; do
      printf '  '
      change_summary "$pkg"
      printf '\n'
    done
    echo ""
    printf "Proceed? [y/N] "
    read -r answer
    if [[ "$answer" != [yY]* ]]; then
      echo "Aborted."
      exit 0
    fi
  fi

  if [[ ! -f "$overrides_file" ]]; then
    echo "error: $overrides_file not found" >&2
    exit 1
  fi

  update_field() {
    local key="$1" value="$2"
    ${sed} -i "s|^\(  $key = \)\".*\";|\1\"$value\";|" "$overrides_file"
  }

  for pkg in "''${outdated[@]}"; do
    case "$pkg" in
      brave)
        echo ""
        echo "''${bold}brave''${reset}: $local_brave -> ''${green}$latest_brave''${reset}"
        brave_url="https://github.com/brave/brave-browser/releases/download/v''${latest_brave}/brave-v''${latest_brave}-darwin-arm64.zip"
        brave_hash=$(prefetch_sri "$brave_url")
        update_field "brave.version" "$latest_brave"
        update_field "brave.hash" "$brave_hash"
        ;;
      chrome)
        echo ""
        echo "''${bold}chrome''${reset}: $local_chrome -> ''${green}$latest_chrome''${reset}"
        uuid=$(${uuidgen})
        post_data="<?xml version='1.0' encoding='UTF-8'?><request protocol='3.0' version='1.3.23.9' shell_version='1.3.21.103' ismachine='1' sessionid='$uuid' installsource='ondemandcheckforupdate' requestid='$uuid' dedup='cr'><hw sse='1' sse2='1' sse3='1' ssse3='1' sse41='1' sse42='1' avx='1' physmemory='12582912' /><os platform='mac' version='$latest_chrome' arch='arm64'/><app appid='com.google.Chrome' ap=' ' version=' ' nextversion=' ' lang=' ' brand='GGLS' client=' '><updatecheck/></app></request>"
        response=$(${curl} -s -X POST -H "Content-Type: text/xml" --data "$post_data" "https://tools.google.com/service/update2")
        chrome_url="$(echo "$response" | ${xmllint} --xpath "string(//url[contains(@codebase, 'http://dl.google.com/release2')]/@codebase)" -)$(echo "$response" | ${xmllint} --xpath "string(//package/@name)" -)"
        chrome_slug=$(echo "$chrome_url" | ${grep} -oP '(?<=chrome/)[^_]+')
        chrome_hash=$(prefetch_sri "$chrome_url")
        update_field "google-chrome.version" "$latest_chrome"
        update_field "google-chrome.slug" "$chrome_slug"
        update_field "google-chrome.hash" "$chrome_hash"
        ;;
      firefox)
        echo ""
        echo "''${bold}firefox''${reset}: $local_firefox -> ''${green}$latest_firefox''${reset}"
        firefox_url="https://archive.mozilla.org/pub/firefox/releases/''${latest_firefox}/source/firefox-''${latest_firefox}.source.tar.xz"
        firefox_sha512=$(${nix}-prefetch-url --type sha512 "$firefox_url" 2>/dev/null)
        update_field "firefox.version" "$latest_firefox"
        update_field "firefox.sha512" "$firefox_sha512"
        ;;
      orbstack)
        echo ""
        echo "''${bold}orbstack''${reset}: $local_orbstack -> ''${green}$latest_orbstack''${reset}"
        orbstack_url="https://cdn-updates.orbstack.dev/arm64/OrbStack_v$(echo "$latest_orbstack" | tr '-' '_')_arm64.dmg"
        orbstack_hash=$(prefetch_sri "$orbstack_url")
        update_field "orbstack.version" "$latest_orbstack"
        update_field "orbstack.hash" "$orbstack_hash"
        ;;
      slack)
        echo ""
        echo "''${bold}slack''${reset}: $local_slack -> ''${green}$latest_slack''${reset}"
        slack_url="https://downloads.slack-edge.com/desktop-releases/mac/arm64/''${latest_slack}/Slack-''${latest_slack}-macOS.dmg"
        slack_hash=$(prefetch_sri "$slack_url")
        update_field "slack.version" "$latest_slack"
        update_field "slack.hash" "$slack_hash"
        ;;
      zoom)
        echo ""
        echo "''${bold}zoom''${reset}: $local_zoom -> ''${green}$latest_zoom''${reset}"
        zoom_url="https://zoom.us/client/''${latest_zoom}/zoomusInstallerFull.pkg?archType=arm64"
        zoom_hash=$(prefetch_sri "$zoom_url" zoomusInstallerFull.pkg)
        update_field "zoom-us.version" "$latest_zoom"
        update_field "zoom-us.hash" "$zoom_hash"
        ;;
    esac
  done

  echo ""
  echo "''${green}Updated $overrides_file''${reset}"
''
