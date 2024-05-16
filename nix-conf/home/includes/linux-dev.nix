{ config, pkgs, ... }:
{
  imports = [
    ./dev-common.nix
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
    extraPackages = (epkgs: [ epkgs.vterm epkgs.multi-vterm ] );
  };

  home.packages = with pkgs; [
    mpv
    mu
    pinentry
    protonvpn-cli
    youtube-dl
  ];

}

