{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./private.nix
    ../modules/base.nix
  ];

  networking.hostName = "djmuk2";

  services.openiscsi.enable = true;

  system.stateVersion = "22.05";
}
