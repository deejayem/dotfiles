{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  sopsFile = ../../. + "/${config.networking.hostName}/secrets.yaml";
  ageFile = ../../. + "/${config.networking.hostName}/private.nix.age";
in
{
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  # This should only contain sensitive information such as email
  # addresses that are not really secrets, but are better kept
  # private
  host.private =
    if builtins.extraBuiltins == null then
      throw "extraBuiltins is not available"
    else if !(builtins.extraBuiltins ? readRageForHost) then
      throw "extraBuiltins.readRageForHost is not available"
    else if !builtins.pathExists ageFile then
      throw "private.nix.age does not exist for ${config.networking.hostName}"
    else
      builtins.extraBuiltins.readRageForHost ageFile;

  environment.systemPackages = with pkgs; [
    sops
    ssh-to-age
  ];

  sops.defaultSopsFile = builtins.path {
    path = sopsFile;
    name = "${config.networking.hostName}-secrets.yaml";
  };
}
