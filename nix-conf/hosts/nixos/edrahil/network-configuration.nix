{ private, ... }:
{
  networking = {
    defaultGateway6 = {
      address = "fe80::1";
      interface = "ens3";
    };

    interfaces = {
      inherit (private.networking.interfaces) ens3;
    };
  };
}
