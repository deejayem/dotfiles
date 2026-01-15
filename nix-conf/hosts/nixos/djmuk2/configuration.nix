{ config, lib, ... }:
{
  imports = [
    ../modules/base.nix
    ./hardware-configuration.nix
  ];

  services.openiscsi = {
    enable = true;
  };

  sops.templates."iscsi-initiatorname" = {
    content = ''
      InitiatorName=${config.sops.placeholder."iscsi/iqn"}
    '';
    mode = "0400";
  };

  environment.etc."iscsi/initiatorname.iscsi" = lib.mkForce {
    source = config.sops.templates."iscsi-initiatorname".path;
  };

  system.stateVersion = "22.05";
}
