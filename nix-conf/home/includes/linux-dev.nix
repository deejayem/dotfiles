{ config, pkgs, ... }:
{
  imports = [
    ./dev-common.nix
  ];
  #services.emacs.package = pkgs.emacsUnstable;

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
  programs.emacs = {
    enable = true;
    #package = pkgs.emacsGcc;
    package = pkgs.emacs28NativeComp;
    extraPackages = (epkgs: [ epkgs.vterm ] );
  };

  home.packages = with pkgs; [
    docker
    docker-compose
    gcc
    gnumake
    mpv
    mu
    notmuch
    nix
    protonvpn-cli
    youtube-dl
  ];

}

