{ config, pkgs, ... }:
{
  imports = [ ./dev-common.nix ];

  host.role = "workstation";

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
