{ ... }:
{
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  services.thermald.enable = true;
  services.power-profiles-daemon.enable = false;
  services.upower.enable = true;
}
