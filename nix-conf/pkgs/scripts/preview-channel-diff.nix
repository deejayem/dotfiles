{ lib, pkgs, ... }:

pkgs.writeShellScriptBin "preview-channel-diff" ''
  set -euo pipefail

  NIX=${lib.getExe pkgs.nix}
  JQ=${lib.getExe pkgs.jq}
  NVD=${lib.getExe pkgs.nvd}

  usage() {
    echo "Usage: preview-channel-diff <nixpkgs-revision> [host]" >&2
    exit 1
  }

  if [ $# -lt 1 ] || [ $# -gt 2 ]; then
    usage
  fi

  REV="$1"
  HOSTNAME="''${2:-$(hostname -s 2>/dev/null || hostname)}"
  FLAKE="''${NH_FLAKE:-$HOME/dotfiles/nix-conf}"

  if ! HOST_METADATA_JSON=$($NIX eval --json "$FLAKE#hostMetadata.$HOSTNAME" 2>/dev/null); then
    echo "Could not resolve host metadata for '$HOSTNAME' from $FLAKE#hostMetadata." >&2
    exit 2
  fi

  OS=$(printf '%s' "$HOST_METADATA_JSON" | $JQ -r '.os // empty')
  VERSION=$(printf '%s' "$HOST_METADATA_JSON" | $JQ -r '.version')
  HOME_NAME=$(printf '%s' "$HOST_METADATA_JSON" | $JQ -r '.homeName')
  NIXPKGS_INPUT=$(printf '%s' "$HOST_METADATA_JSON" | $JQ -r '.nixpkgsInput')

  HOME_ATTR="homeConfigurations.\"$HOME_NAME\".activationPackage"
  OVERRIDE="github:NixOS/nixpkgs/$REV"
  SYSTEM_PROFILE="/nix/var/nix/profiles/system"
  HOME_PROFILE="$HOME/.local/state/nix/profiles/home-manager"

  case "$OS" in
    darwin)
      SYSTEM_ATTR="darwinConfigurations.$HOSTNAME.system"
      ;;
    nixos)
      SYSTEM_ATTR="nixosConfigurations.$HOSTNAME.config.system.build.toplevel"
      ;;
    "")
      SYSTEM_ATTR=""
      ;;
    *)
      echo "Unsupported host OS '$OS' for '$HOSTNAME'." >&2
      exit 3
      ;;
  esac

  if [ -n "$OS" ]; then
    echo "Host: $HOSTNAME ($OS, $VERSION)"
  else
    echo "Host: $HOSTNAME (home-only, $VERSION)"
  fi
  echo "Override: $NIXPKGS_INPUT -> $OVERRIDE"

  if [ -n "$SYSTEM_ATTR" ]; then
    SYSTEM_RESULT=$($NIX build --no-link --print-out-paths \
      "$FLAKE#$SYSTEM_ATTR" \
      --override-input "$NIXPKGS_INPUT" "$OVERRIDE" \
      --no-write-lock-file)

    echo
    echo "System changes"
    $NVD diff "$SYSTEM_PROFILE" "$SYSTEM_RESULT"
  else
    echo
    echo "Skipping system changes"
  fi

  HOME_RESULT=$($NIX build --no-link --print-out-paths \
    "$FLAKE#$HOME_ATTR" \
    --override-input "$NIXPKGS_INPUT" "$OVERRIDE" \
    --no-write-lock-file)

  echo
  echo "Home changes"
  $NVD diff "$HOME_PROFILE" "$HOME_RESULT"
''
