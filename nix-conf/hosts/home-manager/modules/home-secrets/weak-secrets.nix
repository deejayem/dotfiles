{ config, lib, ... }:
let
  secretsLib = import ../../../lib/secrets-indexer.nix { inherit lib; };
  inherit (secretsLib) importPrivateIfExists mkOrg;

  org = mkOrg config.host.org ../orgs;

  basePrivate = importPrivateIfExists ./secrets/private.nix;

  orgPrivate =
    if org != null then
      let
        private = importPrivateIfExists (org.dir + "/secrets/private.nix");
      in
      lib.optionalAttrs (private != { }) { ${org.name} = private; }
    else
      { };
in
{
  # This should only contain sensitive information such as email
  # addresses that are not really secrets, but are better kept
  # private. They are encrypted with git-crypt, and are read
  # at build time, and written to the nix store.
  host.private = basePrivate // orgPrivate;
}
