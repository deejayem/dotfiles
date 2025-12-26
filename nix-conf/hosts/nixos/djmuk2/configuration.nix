{ private, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../modules/base.nix
    ../modules/host-secrets.nix
  ];

  networking.hostName = "djmuk2";

  services.openiscsi = {
    enable = true;
    inherit (private.services.openiscsi) name;
  };

  system.stateVersion = "22.05";
}
