{ ... }:
{
  networking = {
    interfaces.ens3.ipv6.addresses = [{
      # Emulate nix-sops. Technically an anti-pattern, but IP addresses aren't real secrets, and this has to be embedded here,
      # as we cannot set a file path to read it from.
      # Populate/update with:
      # doas su -c "SOPS_AGE_KEY=$(doas ssh-to-age -private-key -i /etc/ssh/ssh_host_ed25519_key) `which sops` -d --extract '[\"ipv6_address\"]' secrets/edrahil.yaml > /root/.config/secrets/ipv6_address"
      address = builtins.readFile "/root/.config/secrets/ipv6_address";
      prefixLength = 64;
    }];
    defaultGateway6 = {
      address = "fe80::1";
      interface = "ens3";
    };
  };
}
