{ config, pkgs, ... }:
{
  imports = [ ./dev-common.nix ];

  host.role = "workstation";

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
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
