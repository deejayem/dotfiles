{
  lib,
  pkgs,
  inputs ? throw "nix-update requires inputs",
  os ? throw "nix-update requires os",
  version ? throw "nix-update requires version",
  ...
}:

let
  nixpkgsInput = "nixpkgs-${version}";
  homeManagerInput = "home-manager-${version}";
in
pkgs.writeShellScriptBin "nix-update" ''
  set -euo pipefail

  NIX_CONF="''${NH_FLAKE:-$HOME/dotfiles/nix-conf}"
  FLAKE_LOCK="$NIX_CONF/flake.lock"
  OS="${os}"
  CURRENT_NIXPKGS_REV="${inputs.${nixpkgsInput}.rev}"
  CURRENT_HM_REV="${inputs.${homeManagerInput}.rev}"
  CURRENT_NIX_DARWIN_REV="${inputs.nix-darwin.rev}"
  JQ=${lib.getExe pkgs.jq}
  NH=${lib.getExe pkgs.nh}
  NIX=${lib.getExe pkgs.nix}

  BACKUP_DIR="''${XDG_STATE_HOME:-$HOME/.local/state}/lock-backups/flake"
  mkdir -p "$BACKUP_DIR"
  TIMESTAMP=$(${lib.getExe' pkgs.coreutils "date"} +%Y-%m-%dT%H-%M-%S)
  cp "$FLAKE_LOCK" "$BACKUP_DIR/flake.lock.$TIMESTAMP"
  ${lib.getExe' pkgs.findutils "find"} "$BACKUP_DIR" -name "flake.lock.*" -mtime +30 -delete

  cd "$NIX_CONF"
  $NIX flake update

  LOCKFILE_NIXPKGS_REV=$($JQ -r '.nodes."${nixpkgsInput}".locked.rev' "$FLAKE_LOCK")
  LOCKFILE_HM_REV=$($JQ -r '.nodes."${homeManagerInput}".locked.rev' "$FLAKE_LOCK")

  OS_NEEDS_UPDATE=false
  HOME_NEEDS_UPDATE=false

  if [ "$OS" = "darwin" ]; then
    LOCKFILE_NIX_DARWIN_REV=$($JQ -r '.nodes."nix-darwin".locked.rev' "$FLAKE_LOCK")

    if [ "$CURRENT_NIX_DARWIN_REV" != "$LOCKFILE_NIX_DARWIN_REV" ]; then
      echo "nix-darwin revision changed: $CURRENT_NIX_DARWIN_REV -> $LOCKFILE_NIX_DARWIN_REV"
      OS_NEEDS_UPDATE=true
      HOME_NEEDS_UPDATE=true
    fi
  fi

  if [ "$CURRENT_NIXPKGS_REV" != "$LOCKFILE_NIXPKGS_REV" ]; then
    echo "nixpkgs revision changed: $CURRENT_NIXPKGS_REV -> $LOCKFILE_NIXPKGS_REV"
    if [ -n "$OS" ]; then
      OS_NEEDS_UPDATE=true
    fi
    HOME_NEEDS_UPDATE=true
  fi

  if [ "$CURRENT_HM_REV" != "$LOCKFILE_HM_REV" ]; then
    echo "home-manager revision changed: $CURRENT_HM_REV -> $LOCKFILE_HM_REV"
    HOME_NEEDS_UPDATE=true
  fi

  case "$OS" in
    darwin)
      if [ "$OS_NEEDS_UPDATE" = true ]; then
        $NH darwin switch -a
      fi
      ;;
    nixos)
      if [ "$OS_NEEDS_UPDATE" = true ]; then
        $NH os switch -a
      fi
      ;;
    "")
      :
      ;;
    *)
      echo "Unsupported host OS '$OS' for '$HOSTNAME'." >&2
      exit 3
      ;;
  esac

  if [ "$HOME_NEEDS_UPDATE" = true ]; then
    $NH home switch -a
  fi

  if [ "$OS_NEEDS_UPDATE" = false ] && [ "$HOME_NEEDS_UPDATE" = false ]; then
    echo "System and home are up to date!"
  fi
''
