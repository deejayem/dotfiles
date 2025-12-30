#!/bin/sh

USE_NIX_PLUGINS=0
[ "$1" = "--plugins" ] && USE_NIX_PLUGINS=1

[ -f ~/.config/sops/age/keys.txt ] || ( echo "Age key not present, aborting." ; exit 1 )

[ -e ~/dotfiles ] || git clone git@codeberg.org:djm/dotfiles.git

NIX_CONF_DIR=$HOME/dotfiles/nix-conf

ln -sf $NIX_CONF_DIR ~/.config/home-manager
ln -sf ~/dotfiles/.emacs.d ~/

export NIX_CONFIG="experimental-features = nix-command flakes"

if [ $USE_NIX_PLUGINS -eq 1 ]; then
  NIX_PLUGINS="$(nix build --no-link --print-out-paths "$NIX_CONF_DIR#nix-plugins")"
  EXTRA_BUILTINS="$(nix store add-file "$NIX_CONF_DIR/nix-plugins/extra-builtins.nix")"
  export NIX_CONFIG="$NIX_CONFIG
plugin-files = $NIX_PLUGINS/lib/nix/plugins
extra-builtins-file = $EXTRA_BUILTINS"
fi

if [ -x "$(command -v nixos-version)" ]; then
  ln -sf $NIX_CONF_DIR /etc/nixos

  nixos-rebuild switch --sudo
else
  [ -x "$(command -v nix)" ] || sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install)

  if [ "$(uname 2> /dev/null)" = "Darwin"  ]; then
    ln -sf $NIX_CONF_DIR /etc/nix-darwin

    sudo nix run nix-darwin/master#darwin-rebuild -- switch
  fi
fi

nix run home-manager/master -- switch --flake $NIX_CONF_DIR

