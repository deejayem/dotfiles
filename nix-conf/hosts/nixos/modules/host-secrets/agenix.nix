{
  config,
  lib,
  pkgs,
  inputs,
  hostname,
  username,
  ...
}:
let
  secretsLib = import ../../../lib/secrets-indexer.nix { inherit lib; };

  secrets = secretsLib.discoverHostSecrets {
    secretType = "age";
    hostDir = ../../${hostname}/secrets;
  };

  agenixPkg = import ../../../lib/agenix-package.nix { inherit lib pkgs inputs; };
in
{
  imports = [ inputs.agenix.nixosModules.default ];
  config = lib.mkIf secrets.hasSecrets {
    age = {
      identityPaths = [ "${config.users.users.${username}.home}/.ssh/agenix" ];
      secrets = secrets.attrs;
    };
    environment.systemPackages = [ agenixPkg ];
  };
}
