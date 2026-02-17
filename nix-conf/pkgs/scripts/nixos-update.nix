{
  lib,
  pkgs,
  inputs ? throw "nixos-update requires inputs",
  version ? throw "nixos-update requires version",
  ...
}:

let
  nixpkgsInput = "nixpkgs-${version}";
  hmInput = "home-manager-${version}";
in
pkgs.writeShellScriptBin "nixos-update" ''
  set -euo pipefail

  NIX_CONF="$HOME/dotfiles/nix-conf/"
  FLAKE_LOCK="$NIX_CONF/flake.lock"
  JQ=${lib.getExe pkgs.jq}
  NH=${lib.getExe pkgs.nh}
  NIX=${lib.getExe pkgs.nix}

  BACKUP_DIR="''${XDG_STATE_HOME:-$HOME/.local/state}/lock-backups/flake"
  mkdir -p "$BACKUP_DIR"
  TIMESTAMP=$(${lib.getExe' pkgs.coreutils "date"} +%Y-%m-%dT%H-%M-%S)
  cp "$FLAKE_LOCK" "$BACKUP_DIR/flake.lock.$TIMESTAMP"
  ${lib.getExe' pkgs.findutils "find"} "$BACKUP_DIR" -name "flake.lock.*" -mtime +30 -delete

  cd $NIX_CONF
  $NIX flake update

  CURRENT_NIXPKGS_REV="${inputs.${nixpkgsInput}.rev}"
  CURRENT_HM_REV="${inputs.${hmInput}.rev}"

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
