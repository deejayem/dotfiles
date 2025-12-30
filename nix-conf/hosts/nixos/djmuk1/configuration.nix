{ ... }:
{
  imports = [
    ../modules/base.nix
    ./hardware-configuration.nix
  ];

  networking.hostName = "djmuk1";

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 2 * 1024;
    }
  ];

  system.stateVersion = "23.11";
}
