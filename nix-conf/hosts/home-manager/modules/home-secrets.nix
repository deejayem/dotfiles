{ config, ... }:
{
  imports = [
    ./programs/sops.nix
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
