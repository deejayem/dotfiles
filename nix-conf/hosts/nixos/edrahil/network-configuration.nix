{ config, ... }:
{
  networking = {
    defaultGateway6 = {
      address = "fe80::1";
      interface = "ens3";
    };

    interfaces = {
      inherit (config.host.private.networking.interfaces) ens3;
    };
  };
}
