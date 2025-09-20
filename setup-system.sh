#!/bin/sh

[ -f ~/.config/sops/age/keys.txt ] || ( echo "Age key not present, aborting." ; exit 1 )

[ -e ~/dotfiles ] || git clone git@codeberg.org:djm/dotfiles.git

NIX_CONF=$HOME/dotfiles/nix-conf

ln -sf $NIX_CONF ~/.config/home-manager
ln -sf ~/dotfiles/.p10k.zsh ~/
ln -sf ~/dotfiles/.emacs.d ~/

if [ -x "$(command -v nixos-version)" ]; then
  ln -sf $NIX_CONF /etc/nixos

  nixos-rebuild --extra-experimental-features nix-command --extra-experimental-features switch --use-remote-sudo
else
  [ -x "$(command -v nix)" ] || sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install)

  if [ "$(uname 2> /dev/null)" = "Darwin"  ]; then
    ln -sf $NIX_CONF /etc/nix-darwin

    sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin/master#darwin-rebuild -- switch
  fi
fi

nix run home-manager/master -- switch --flake ~/dotfiles/nix-conf

