{
  config,
  pkgs,
  inputs,
  ...
}:
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

  _module.args.private =
    if builtins.extraBuiltins == null then
      throw "extraBuiltins is not available"
    else if !(builtins.extraBuiltins ? readSopsForKey) then
      throw "extraBuiltins.readSopsForKey is not available"
    else if !builtins.pathExists config.sops.defaultSopsFile then
      throw "secrets.yaml does not exist"
    else
      builtins.extraBuiltins.readSopsForKey config.sops.age.keyFile config.sops.defaultSopsFile;
}
