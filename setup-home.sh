#!/bin/sh

[ -f ~/.config/sops/age/keys.txt ] || ( echo "Age key not present, aborting." ; exit 1 )

[ -e ~/dotfiles ] || git clone git@codeberg.org:djm/dotfiles.git

if [ -x "$(command -v nixos-version)" ]; then
  # On NixOS use the hm version corresponding to the NixOS version, and add nixos-unstable as unstable
  nix-channel --add https://github.com/nix-community/home-manager/archive/release-$(nixos-version | cut -d. -f1-2).tar.gz home-manager
  nix-channel --add https://nixos.org/channels/nixos-unstable unstable
else
  # On other systems use master, and add nixpkgs-unstable as unstable for compatibility between NixOS and non-NixOS installations
  nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
  nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
fi

nix-channel --add https://github.com/Mic92/sops-nix/archive/master.tar.gz sops-nix

nix-channel --update

export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
nix-shell '<home-manager>' -A install

HOME_CONF="$HOME/dotfiles/nix-conf/home/${CONF:-${HOST}}.nix"
[ -f $HOME_CONF ] && ln -sf $HOME_CONF ~/.config/home-manager/home.nix
ln -sf ~/dotfiles/.p10k.zsh ~/
ln -sf ~/dotfiles/.emacs.d ~/

home-manager switch

if [ "$(uname 2> /dev/null)" = "Darwin"  ]; then
  ln -sf ~/.nix-profile/Applications/Emacs.app /Applications/
  ln -sf ~/.nix-profile/lib/pam /usr/local/lib/pam
fi
