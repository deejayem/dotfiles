{ pkgs, inputs }:
pkgs.writeShellScriptBin "darwin-update" ''
  set -euo pipefail

  NIX_CONF="$HOME/dotfiles/nix-conf/"
  FLAKE_LOCK="$NIX_CONF/flake.lock"
  JQ=${pkgs.jq}/bin/jq
  NH=${pkgs.nh}/bin/nh
  NIX=${pkgs.nix}/bin/nix

  cd $NIX_CONF
  $NIX flake update

  CURRENT_NIX_DARWIN_REV="${inputs.nix-darwin.rev}"
  CURRENT_NIXPKGS_REV="${inputs.nixpkgs-unstable.rev}"
  CURRENT_HM_REV="${inputs.home-manager-unstable.rev}"

  LOCKFILE_NIX_DARWIN_REV=$($JQ -r '.nodes."nix-darwin".locked.rev' "$FLAKE_LOCK")
  LOCKFILE_NIXPKGS_REV=$($JQ -r '.nodes."nixpkgs-unstable".locked.rev' "$FLAKE_LOCK")
  LOCKFILE_HM_REV=$($JQ -r '.nodes."home-manager-unstable".locked.rev' "$FLAKE_LOCK")

  DARWIN_NEEDS_UPDATE=false
  HOME_NEEDS_UPDATE=false

  if [ "$CURRENT_NIX_DARWIN_REV" != "$LOCKFILE_NIX_DARWIN_REV" ]; then
    echo "nix-darwin revision changed: $CURRENT_NIX_DARWIN_REV → $LOCKFILE_NIX_DARWIN_REV"
    DARWIN_NEEDS_UPDATE=true
    HOME_NEEDS_UPDATE=true
  fi

  if [ "$CURRENT_NIXPKGS_REV" != "$LOCKFILE_NIXPKGS_REV" ]; then
    echo "nixpkgs revision changed: $CURRENT_NIXPKGS_REV → $LOCKFILE_NIXPKGS_REV"
    DARWIN_NEEDS_UPDATE=true
    HOME_NEEDS_UPDATE=true
  fi

  if [ "$CURRENT_HM_REV" != "$LOCKFILE_HM_REV" ]; then
    echo "home-manager revision changed: $CURRENT_HM_REV → $LOCKFILE_HM_REV"
    HOME_NEEDS_UPDATE=true
  fi

  if [ "$DARWIN_NEEDS_UPDATE" = true ]; then
    $NH darwin switch -a
  fi

  if [ "$HOME_NEEDS_UPDATE" = true ]; then
    $NH home switch -a
  fi

  if [ "$DARWIN_NEEDS_UPDATE" = false ] && [ "$HOME_NEEDS_UPDATE" = false ]; then
    echo "nix-darwin and home are up to date!"
  fi
''
