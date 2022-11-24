#!/bin/sh

[ -e ~/dotfiles ] || git clone git@codeberg.org:djm/dotfiles.git

nix-channel --add https://github.com/nix-community/home-manager/archive/release-$(nixos-version | cut -d. -f1-2).tar.gz home-manager
nix-channel --update

export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
nix-shell '<home-manager>' -A install

HOME_CONF="$HOME/dotfiles/nix-conf/home/${CONF:-${HOST}}.nix"
[ -f $HOME_CONF ] && ln -sf $HOME_CONF ~/.config/nixpkgs/home.nix
ln -sf ~/dotfiles/.p10k.zsh ~/
ln -sf ~/dotfiles/.emacs.d ~/

mkdir ~/.config/nix
echo "extra-experimental-features = nix-command flakes" > ~/.config/nix/nix.conf

echo -n $EMAIL > ~/email.txt

home-manager switch

