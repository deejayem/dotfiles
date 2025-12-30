{ config, inputs, ... }:
{
  imports = [
    ../modules/base.nix
    ../modules/host-secrets.nix
    ./hardware-configuration.nix
    ./network-configuration.nix
    ./backups.nix
  ];

  networking.hostName = "edrahil";

  networking.firewall.allowedTCPPorts = [ 2222 ];

  services.openssh.ports = [ 2222 ];
  services.openssh.allowSFTP = true;

  system.stateVersion = "22.05";
}
