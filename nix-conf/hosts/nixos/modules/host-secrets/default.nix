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

  sopsDir = ../../. + "/${hostname}/secrets/sops";
  hasSopsDir = builtins.pathExists sopsDir;

  sopsFiles =
    if hasSopsDir
    then lib.fileset.toList (lib.fileset.fileFilter (f: f.hasExt "yml") sopsDir)
    else [];

  mkSopsSecret = path:
    let
      relativePath  = lib.removePrefix (toString sopsDir + "/") (toString path);
      secretName = lib.removeSuffix ".yml" relativePath;
    in
    lib.nameValuePair secretName {
      sopsFile = path;
      key = "value";
    };
in
{
  imports = [
    inputs.agenix.nixosModules.default
    inputs.sops-nix.nixosModules.sops
  ];

  sops = lib.mkIf hasSopsDir {
    secrets = builtins.listToAttrs (map mkSopsSecret sopsFiles);
  };

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
    pkgs.sops
  ];
}
