{ config, ... }:
{
  imports = [
    ../modules/base.nix
    ../modules/services/identd.nix
    ./hardware-configuration.nix
    ./backups.nix
  ];

  networking.firewall.allowedTCPPorts = [ 2222 ];

  services.openssh.ports = [ 2222 ];
  services.openssh.allowSFTP = true;

  system.stateVersion = "22.05";
}
