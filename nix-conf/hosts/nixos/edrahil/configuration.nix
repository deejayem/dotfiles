{ config, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./network-configuration.nix
    ./backups.nix
    ../modules/base.nix
    ../modules/host-secrets.nix
  ];

  networking.hostName = "edrahil";

  networking.firewall.allowedTCPPorts = [ 2222 ];

  services.openssh.ports = [ 2222 ];
  services.openssh.allowSFTP = true;

  system.stateVersion = "22.05";
}
