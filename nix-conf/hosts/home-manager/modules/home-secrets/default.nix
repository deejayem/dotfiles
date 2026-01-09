{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  privateFile = ./private.nix.age;

  secretsDir = ./secrets;
  ageFiles = lib.fileset.toList (lib.fileset.fileFilter (f: f.hasExt "age") secretsDir);

  mkSecretEntry =
    path:
    let
      relativePath = lib.removePrefix (toString secretsDir + "/") (toString path);
      secretName = lib.removeSuffix ".age" relativePath;
    in
    lib.nameValuePair secretName { file = path; };
in
{
  imports = [
    inputs.agenix.homeManagerModules.default
  ];

  age = {
    secretsDir = "${config.xdg.stateHome}/agenix";
    identityPaths = [ "${config.home.homeDirectory}/.ssh/agenix" ];
    secrets = builtins.listToAttrs (map mkSecretEntry ageFiles);
  };

  home.packages = with pkgs; [
    (inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
      ageBin = "${lib.getExe pkgs.rage}";
    })
  ];

  # This should only contain sensitive information such as email
  # addresses that are not really secrets, but are better kept
  # private
  host.private =
    if builtins.extraBuiltins == null then
      throw "extraBuiltins is not available"
    else if !(builtins.extraBuiltins ? readRageForHost) then
      throw "extraBuiltins.readRageForKey is not available"
    else if !builtins.pathExists privateFile then
      throw "private.nix.age does not exist for ${config.networking.hostName}"
    else
      builtins.extraBuiltins.readRageForKey "${config.home.homeDirectory}/.ssh/agenix" privateFile;
}
