{ config, pkgs, ... }:
{
  imports = [ ./dev-common.nix ];

  programs.emacs = {
    enable = true;
    extraPackages = (
      epkgs: [
        epkgs.vterm
        epkgs.multi-vterm
      ]
    );
  };

  home.packages = with pkgs; [
    lame
    libtree
    mp3cat
    mu
    pinentry
    protonvpn-cli
    sword
    yt-dlp
  ];

}
