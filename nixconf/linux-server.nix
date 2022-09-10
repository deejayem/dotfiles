{ config, pkgs, ... }:
{

  #services.emacs.package = pkgs.emacsUnstable;

  home.packages = with pkgs; [
    bitlbee
    bitlbee-discord
    emacs-nox
    heroku
    irssi
  ];

}

