#!/bin/sh

[ -f ~/.ssh/agenix ] || ( echo "Agenix ssh key not present, aborting." ; exit 1 )

[ -e ~/dotfiles ] || git clone git@codeberg.org:djm/dotfiles.git

NIX_CONF=$HOME/dotfiles/nix-conf

ln -sf $NIX_CONF ~/.config/home-manager
ln -sf ~/dotfiles/.emacs.d ~/

if [ -x "$(command -v nixos-version)" ]; then
  ln -sf $NIX_CONF /etc/nixos

  nixos-rebuild --extra-experimental-features nix-command --extra-experimental-features switch --use-remote-sudo
else
  [ -x "$(command -v nix)" ] || sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install)

  if [ "$(uname 2> /dev/null)" = "Darwin"  ]; then
    ln -sf $NIX_CONF /etc/nix-darwin

    sudo xcode-select --install

    sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin/master#darwin-rebuild -- switch
  fi
fi

nix run home-manager/master -- switch --flake ~/dotfiles/nix-conf

atuin import zsh

atuin_user=$(rage -i ~/.ssh/agenix -d "${NIX_CONF}/hosts/home-manager/modules/home-secrets/secrets/age/atuin/user.age")
atuin_password=$(rage -i ~/.ssh/agenix -d "${NIX_CONF}/hosts/home-manager/modules/home-secrets/secrets/age/atuin/password.age")
atuin_key=$(rage -i ~/.ssh/agenix -d "${NIX_CONF}/hosts/home-manager/modules/home-secrets/secrets/age/atuin/key.age")

atuin login -u "$atuin_user" -p "$atuin_password" -k "$atuin_key"
