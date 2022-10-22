{ config, pkgs, ... }:
{
  imports = [
    ./common.nix
  ];

  home.packages = with pkgs; [
    emacs-nox
    irssi
    msmtp
    neomutt
  ];

  ## TODO tmux
}

