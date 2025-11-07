{ config, inputs, ... }:
let
  private = import ./private.nix;
in
{
  _module.args = { inherit private; };

  imports = [
    ./hardware-configuration.nix
    ./network-configuration.nix
    ./backups.nix
    ../modules/base.nix
    inputs.sops-nix.nixosModules.sops
  ];

  networking.hostName = "edrahil";

  networking.firewall.allowedTCPPorts = [ 2222 ];

  services.openssh.ports = [ 2222 ];
  services.openssh.allowSFTP = true;

  sops.defaultSopsFile = builtins.path {
    path = ./secrets.yaml;
    name = "${config.networking.hostName}-secrets.yaml";
  };

  system.stateVersion = "22.05";
}
