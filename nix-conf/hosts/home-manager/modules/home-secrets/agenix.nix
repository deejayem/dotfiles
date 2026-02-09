{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  secretsLib = import ../../../lib/secrets-indexer.nix { inherit lib; };

  sshKey = "${config.home.homeDirectory}/.ssh/agenix";
  rage = lib.getExe pkgs.rage;
  agenixPkg = inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
    ageBin = rage;
  };
  agenixCli = lib.getExe' agenixPkg "agenix";

  secrets = secretsLib.discoverHomeSecrets {
    secretType = "age";
    baseDir = ./secrets;
    org = secretsLib.mkOrg config.host.org ../orgs;
  };

  agenixEdit = pkgs.writeShellScriptBin "agenix-edit" ''
    set -euo pipefail
    if [ "$#" -ne 1 ]; then
      echo "usage: agenix-edit <path/to/secret.age>" >&2
      exit 2
    fi
    secret="$1"
    if [ ! -f "$secret" ]; then
      echo "Secret does not exist; creating empty encrypted file."
      mkdir -p "$(dirname "$secret")"
      ${rage} -e -i ${sshKey} -o "$secret" /dev/null
    fi
    ${agenixCli} -i ${sshKey} -e "$secret"
  '';
in
{
  imports = [ inputs.agenix.homeManagerModules.default ];
  config = lib.mkIf secrets.hasSecrets {
    age = {
      secretsDir = "${config.xdg.stateHome}/agenix";
      identityPaths = [ sshKey ];
      secrets = secrets.attrs;
    };
    home.packages = [
      agenixEdit
      agenixPkg
    ];
  };
}
