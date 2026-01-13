{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  ageDir = ./secrets/age;
  ageDirExists = builtins.pathExists ageDir;

  sshKey = "${config.home.homeDirectory}/.ssh/agenix";
  rage = lib.getExe pkgs.rage;
  agenixPkg = (
    inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
      ageBin = rage;
    }
  );
  agenixCli = lib.getExe' agenixPkg "agenix";

  ageFiles =
    if ageDirExists then
      lib.fileset.toList (lib.fileset.fileFilter (f: f.hasExt "age") ageDir)
    else
      [ ];

  mkSecretEntry =
    path:
    let
      relativePath = lib.removePrefix (toString ageDir + "/") (toString path);
      secretName = lib.removeSuffix ".age" relativePath;
    in
    lib.nameValuePair secretName { file = path; };

  agenixEdit = pkgs.writeShellScriptBin "agenix-edit" ''
    set -e

    if [ "$#" -ne 1 ]; then
      echo "usage: agenix-edit <path/to/secret.age>" >&2
      exit 2
    fi

    secret="$1"

    if [ ! -f "$secret" ]; then
      echo "Secret does not exist"
      mkdir -p "$(dirname "$secret")"
      printf "" | ${rage} -e -i ${sshKey} -o "$secret"
    else
      echo "Secret does exix"
    fi

    ${agenixCli} -i ${sshKey} -e "$secret"
  '';
in
{
  imports = [ inputs.agenix.homeManagerModules.default ];

  config = lib.mkIf ageDirExists {

    age = {
      secretsDir = "${config.xdg.stateHome}/agenix";
      identityPaths = [ sshKey ];
      secrets = builtins.listToAttrs (map mkSecretEntry ageFiles);
    };

    home.packages = [
      agenixEdit
      agenixPkg
    ];
  };
}
