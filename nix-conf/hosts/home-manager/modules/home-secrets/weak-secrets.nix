{ lib, ... }:
let
  secretsLib = import ../../../lib/secrets-indexer.nix { inherit lib; };
  inherit (secretsLib) importPrivateIfExists;

  orgsDir = ../orgs;

  basePrivate = importPrivateIfExists ./secrets/private.nix;

  orgPrivates = lib.optionalAttrs (builtins.pathExists orgsDir) (
    let
      orgNames = lib.attrNames (lib.filterAttrs (_: t: t == "directory") (builtins.readDir orgsDir));
      readOrgPrivate =
        org:
        let
          orgPrivate = importPrivateIfExists (orgsDir + "/${org}/secrets/private.nix");
        in
        lib.optional (orgPrivate != { }) (lib.nameValuePair org orgPrivate);
    in
    builtins.listToAttrs (lib.concatMap readOrgPrivate orgNames)
  );
in
{
  # This should only contain sensitive information such as email
  # addresses that are not really secrets, but are better kept
  # private. They are encrypted with git-crypt, and are read
  # at build time, and written to the nix store.
  host.private = basePrivate // orgPrivates;
}
