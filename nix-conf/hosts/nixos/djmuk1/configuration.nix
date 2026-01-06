{ ... }:
{
  imports = [
    ../modules/base.nix
    ./hardware-configuration.nix
  ];

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 2 * 1024;
    }
  ];

  system.stateVersion = "23.11";
}
