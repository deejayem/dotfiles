{
  config,
  pkgs,
  inputs,
  ...
}:
let
  ageFile = ./private.nix.age;
in
{
  imports = [
    inputs.sops-nix.homeManagerModules.sops
  ];

  sops = {
    age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
    defaultSopsFile = builtins.path {
      path = ./secrets.yaml;
      name = "home-secrets.yaml";
    };
  };

  home.packages = with pkgs; [
    sops
  ];

  # This should only contain sensitive information such as email
  # addresses that are not really secrets, but are better kept
  # private
  host.private =
    if builtins.extraBuiltins == null then
      throw "extraBuiltins is not available"
    else if !(builtins.extraBuiltins ? readRageForHost) then
      throw "extraBuiltins.readRageForKey is not available"
    else if !builtins.pathExists ageFile then
      throw "private.nix.age does not exist for ${config.networking.hostName}"
    else
      builtins.extraBuiltins.readRageForKey config.sops.age.keyFile ageFile;
}
