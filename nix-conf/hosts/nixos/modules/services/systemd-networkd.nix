{
  config,
  lib,
  ...
}:
let
  hasStaticV6 = config.sops.secrets ? "network/v6";
in
{
  networking.useDHCP = false;

  systemd.network = {
    enable = true;

    networks."20-en-default" = {
      matchConfig.Name = "en*";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = false;
      };
    };

    networks."10-static-v6" = lib.mkIf hasStaticV6 {
      matchConfig.Name = "ens3";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = false;
      };
      routes = [
        { Gateway = "fe80::1"; }
      ];
    };
  };

  sops.templates."10-static-v6.network.d/10-address.conf" = lib.mkIf hasStaticV6 {
    content = ''
      [Address]
      Address=${config.sops.placeholder."network/v6"}
      DuplicateAddressDetection=none
    '';
    # There's no reason for this not to be world readable
    mode = "0444";
  };

  environment.etc."systemd/network/10-static-v6.network.d/10-address.conf" = lib.mkIf hasStaticV6 {
    source = config.sops.templates."10-static-v6.network.d/10-address.conf".path;
  };
}
