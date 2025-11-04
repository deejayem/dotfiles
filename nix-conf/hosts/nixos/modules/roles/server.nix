{ ... }:
{
  zramSwap.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 113 ];
  };

  services.oidentd.enable = true;

  nix.optimise.automatic = true;
  nix.optimise.dates = [ "03:00" ];
}
