{ config, inputs, ... }:
{
  imports = [
    ../modules/base.nix
    ./hardware-configuration.nix
    ./backups.nix
  ];

  networking.firewall.allowedTCPPorts = [ 2222 ];

  services.openssh.ports = [ 2222 ];
  services.openssh.allowSFTP = true;

  system.stateVersion = "22.05";
}
