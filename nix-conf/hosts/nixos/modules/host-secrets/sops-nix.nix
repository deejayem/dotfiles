{
  lib,
  inputs,
  hostname,
  ...
}:
let
  secretsLib = import ../../../lib/secrets-indexer.nix { inherit lib; };

  secrets = secretsLib.discoverHostSecrets {
    secretType = "sops";
    hostDir = ../../${hostname}/secrets;
  };
in
{
  imports = [ inputs.sops-nix.nixosModules.sops ];
  config = lib.mkIf secrets.hasSecrets {
    sops.secrets = secrets.attrs;
  };
}
