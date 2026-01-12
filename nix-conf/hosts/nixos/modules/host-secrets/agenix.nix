{
  config,
  lib,
  pkgs,
  inputs,
  hostname,
  ...
}:
let
  ageDir = ../../${hostname}/secrets/age;
  ageDirExists = builtins.pathExists ageDir;

  ageFiles =
    if ageDirExists then
      lib.fileset.toList (lib.fileset.fileFilter (f: f.hasExt "age") ageDir)
    else
      [ ];

  mkAgeSecret =
    path:
    let
      relativePath = lib.removePrefix (toString ageDir + "/") (toString path);
      secretName = lib.removeSuffix ".age" relativePath;
    in
    lib.nameValuePair secretName { file = path; };
in
{
  imports = [ inputs.agenix.nixosModules.default ];

  config = lib.mkIf ageDirExists {
    age = {
      identityPaths = [ "${config.users.users.djm.home}/.ssh/agenix" ];
      secrets = builtins.listToAttrs (map mkAgeSecret ageFiles);
    };

    environment.systemPackages = [
      (inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
        ageBin = "${lib.getExe pkgs.rage}";
      })
    ];
  };
}
