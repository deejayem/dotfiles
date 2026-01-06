{ config, ... }:
{
  imports = [
    ../modules/base.nix
    ../modules/host-secrets
    ./hardware-configuration.nix
  ];

  services.openiscsi = {
    enable = true;
    inherit (config.host.private.services.openiscsi) name;
  };

  system.stateVersion = "22.05";
}
