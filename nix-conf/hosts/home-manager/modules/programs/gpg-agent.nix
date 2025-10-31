{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.gpg-agent = {
    enable = true;
    pinentry.package = lib.mkIf (config.host.role == "server") pkgs.pinentry-curses;
    defaultCacheTtl = 34560000;
    maxCacheTtl = 34560000;
  };
}
