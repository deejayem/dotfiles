{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  secretsLib = import ../../../lib/secrets-indexer.nix { inherit lib; };

  secrets = secretsLib.discoverHomeSecrets {
    secretType = "sops";
    baseDir = ./secrets;
    org = secretsLib.mkOrg config.host.org ../orgs;
  };
in
{
  imports = [ inputs.sops-nix.homeManagerModules.sops ];
  config = lib.mkIf secrets.hasSecrets {
    sops = {
      age.sshKeyPaths = [ "${config.home.homeDirectory}/.ssh/agenix" ];
      secrets = secrets.attrs;
    };
    home.packages = [ pkgs.sops ];
  };
}
