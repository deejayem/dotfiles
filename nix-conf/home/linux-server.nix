{ config, pkgs, ... }:
{

  home.packages = with pkgs; [
    bitlbee
    bitlbee-discord
    emacs-nox
    irssi
  ];

}

