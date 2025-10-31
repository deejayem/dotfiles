{ pkgs, ... }:
{
  home.packages = with pkgs; [
    libtree
    msmtp
    restic
    sword
    yt-dlp
  ];
}
