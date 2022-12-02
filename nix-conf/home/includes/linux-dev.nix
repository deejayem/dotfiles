{ config, pkgs, ... }:
{
  imports = [
    ./dev-common.nix
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs28NativeComp;
    extraPackages = (epkgs: [ epkgs.vterm ] );
  };

  home.packages = with pkgs; [
    mpv
    mu
    protonvpn-cli
    youtube-dl
  ];

}

