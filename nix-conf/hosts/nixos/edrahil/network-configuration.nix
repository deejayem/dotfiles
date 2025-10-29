{ ... }:
{
  imports = [
    ./private.nix
  ];

  # networking.interfaces.ens3.ipv6.addresses configured in private.nix
  networking.defaultGateway6 = {
    address = "fe80::1";
    interface = "ens3";
  };
}
