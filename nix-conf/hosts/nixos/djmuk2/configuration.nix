{ config, ... }:
{
  imports = [
    ../modules/base.nix
    ../modules/host-secrets
    ./hardware-configuration.nix
  ];

  networking.hostName = "djmuk2";

  services.openiscsi = {
    enable = true;
    inherit (config.host.private.services.openiscsi) name;
  };

  system.stateVersion = "22.05";
}
