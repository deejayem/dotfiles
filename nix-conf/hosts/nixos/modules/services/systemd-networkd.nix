{
  config,
  lib,
  hostname,
  ...
}:
let
  # TODO: which is better? Checking if the secret exists make more sense semantically
  # but checking the path might be more robust: E.g. I think config.sops won't exist
  # if the secrets/sops directory doesn't exist for a host, but if (config ? sops) is
  # false, that could fail silently and unexpectedly, e.g. could that happen if
  # there's a module loading order issue?
  #hasStaticV6 = (config ? sops) && (config.sops.secrets ? "network/v6");
  hasStaticV6 = builtins.pathExists ../../${hostname}/secrets/sops/network/v6.yml;
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
