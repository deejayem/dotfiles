{ ... }:
{

  imports = [ ../services/systemd-networkd.nix ];

  zramSwap.enable = true;

  networking.firewall.enable = true;

  nix.optimise = {
    automatic = true;
    dates = [ "03:00" ];
  };
}
