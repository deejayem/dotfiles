{
  config,
  lib,
  pkgs,
  inputs,
  hostname,
  ...
}:
let
  secretsLib = import ../../../lib/secrets-indexer.nix { inherit lib; };

  secrets = secretsLib.discoverHostSecrets {
    secretType = "age";
    hostDir = ../../${hostname}/secrets;
  };

  agenixPkg = inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
    ageBin = lib.getExe pkgs.rage;
  };
in
{
  imports = [ inputs.agenix.nixosModules.default ];
  config = lib.mkIf secrets.hasSecrets {
    age = {
      identityPaths = [ "${config.users.users.djm.home}/.ssh/agenix" ];
      secrets = secrets.attrs;
    };
    environment.systemPackages = [ agenixPkg ];
  };
}
