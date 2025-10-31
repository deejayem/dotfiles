{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ./common.nix ];

  host.role = "server";

  home.packages = with pkgs; [
    emacs-nox # will go in emacs.nix
    libtree # common, when not darwin
    msmtp # remove
    restic # common
    sword # common
    yt-dlp # common
  ];
}
