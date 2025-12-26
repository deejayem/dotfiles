{ ... }:
let
  secrets = builtins.extraBuiltins.readSopsForHost ./secrets.yaml;
in
{
  imports = [
    ./hardware-configuration.nix
    ../modules/base.nix
  ];

  networking.hostName = "djmuk2";

  services.openiscsi = {
    enable = true;
    inherit (secrets.services.openiscsi) name;
  };

  system.stateVersion = "22.05";
}
