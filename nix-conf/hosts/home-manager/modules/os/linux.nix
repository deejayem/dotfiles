{ pkgs, ... }:
{
  imports = [ ../programs/gpg-agent.nix ];

  home.packages = with pkgs; [
    libtree
    msmtp
    restic
    sword
    yt-dlp
  ];
}
