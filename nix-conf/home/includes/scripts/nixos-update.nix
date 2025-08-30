{ pkgs, inputs, systemType }:
let
  currentNixpkgsRev = if systemType == "unstable" then inputs.nixpkgs-unstable.rev else inputs.nixpkgs.rev;
  currentHMRev = if systemType == "unstable" then inputs.home-manager-unstable.rev else inputs.home-manager.rev;

  nixpkgsInput = if systemType == "unstable" then "nixpkgs-unstable" else "nixpkgs";
  hmInput = if systemType == "unstable" then "home-manager-unstable" else "home-manager";
in
pkgs.writeShellScriptBin "nixos-update" ''
  set -euo pipefail

  NIX_CONF="$HOME/dotfiles/nix-conf/"
  FLAKE_LOCK="$NIX_CONF/flake.lock"
  JQ=${pkgs.jq}/bin/jq
  NH=${pkgs.nh}/bin/nh
  NIX=${pkgs.nix}/bin/nix

  cd $NIX_CONF
  $NIX flake update

  CURRENT_NIXPKGS_REV="${currentNixpkgsRev}"
  CURRENT_HM_REV="${currentHMRev}"

  LOCKFILE_NIXPKGS_REV=$($JQ -r '.nodes."${nixpkgsInput}".locked.rev' "$FLAKE_LOCK")
  LOCKFILE_HM_REV=$($JQ -r '.nodes."${hmInput}".locked.rev' "$FLAKE_LOCK")

  OS_NEEDS_UPDATE=false
  HOME_NEEDS_UPDATE=false

  if [ "$CURRENT_NIXPKGS_REV" != "$LOCKFILE_NIXPKGS_REV" ]; then
    echo "nixpkgs revision changed: $CURRENT_NIXPKGS_REV → $LOCKFILE_NIXPKGS_REV"
    OS_NEEDS_UPDATE=true
    HOME_NEEDS_UPDATE=true
  fi

  if [ "$CURRENT_HM_REV" != "$LOCKFILE_HM_REV" ]; then
    echo "home-manager revision changed: $CURRENT_HM_REV → $LOCKFILE_HM_REV"
    HOME_NEEDS_UPDATE=true
  fi

  if [ "$OS_NEEDS_UPDATE" = true ]; then
    $NH os switch -a
  fi

  if [ "$HOME_NEEDS_UPDATE" = true ]; then
    $NH home switch -a
  fi

  if [ "$OS_NEEDS_UPDATE" = false ] && [ "$HOME_NEEDS_UPDATE" = false ]; then
    echo "System and home are up to date!"
  fi
''

