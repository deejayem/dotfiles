{ ... }:
let
  private = import ./private.nix;
in
{
  imports = [
    ./hardware-configuration.nix
    ../modules/base.nix
  ];

  networking.hostName = "djmuk2";

  services.openiscsi = {
    enable = true;
    inherit (private.services.openiscsi) name;
  };

  system.stateVersion = "22.05";
}
