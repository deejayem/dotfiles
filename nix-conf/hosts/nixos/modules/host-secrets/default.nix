{
  config,
  lib,
  pkgs,
  inputs,
  hostname,
  ...
}:
let
  ageFile = ../../. + "/${hostname}/private.nix.age";
in
{
  imports = [
    inputs.agenix.nixosModules.default
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
      throw "private.nix.age does not exist for ${hostname}"
    else
      builtins.extraBuiltins.readRageForHost ageFile;

  environment.systemPackages = [
    (inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default.override { ageBin = "${lib.getExe pkgs.rage}"; })
  ];
}
