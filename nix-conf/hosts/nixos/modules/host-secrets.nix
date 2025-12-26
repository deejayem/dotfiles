{
  config,
  pkgs,
  inputs,
  ...
}:
let
  sopsFile = ../${config.networking.hostName}/secrets.yaml;
in
{
  # This should only contain sensitive information such as email
  # addresses that are not really secrets, but are better kept
  # private
  _module.args.private =
    if builtins.extraBuiltins == null || !(builtins.extraBuiltins ? readSopsForHost) then
      throw "extra-builtins is not set up correctly"
    else if !builtins.pathExists sopsFile then
      throw "secrets.yaml does not exist for ${config.networking.hostName}"
    else
      builtins.extraBuiltins.readSopsForHost sopsFile;

  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  environment.systemPackages = with pkgs; [
    sops
    ssh-to-age
  ];

  sops.defaultSopsFile = builtins.path {
    path = sopsFile;
    name = "${config.networking.hostName}-secrets.yaml";
  };
}
