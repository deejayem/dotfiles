{ lib, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [ 113 ];

  services.oidentd.enable = true;

  systemd.services.oidentd.script= lib.mkForce "${pkgs.oidentd}/sbin/oidentd -u oidentd -g oidentd --limit 32";
}
