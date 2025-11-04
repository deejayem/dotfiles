{ pkgs, ... }:
{
  services.printing.enable = true;
  services.printing.drivers = [
    pkgs.gutenprint
    #pkgs.hplipWithPlugin
  ];

  hardware.sane.enable = true;
}
