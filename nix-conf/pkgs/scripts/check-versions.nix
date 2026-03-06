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

  nix_ver() {
    ${nix} eval --raw "$1" 2>/dev/null
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

  {
    echo "PACKAGE NIXPKGS LOCAL LATEST "
    echo "chrome $nixpkgs_chrome $local_chrome $latest_chrome"
    echo "firefox $nixpkgs_firefox $local_firefox $latest_firefox"
    echo "slack $nixpkgs_slack $local_slack $latest_slack"
    echo "zoom $nixpkgs_zoom $local_zoom $latest_zoom"
  } | ${column} -t
''
