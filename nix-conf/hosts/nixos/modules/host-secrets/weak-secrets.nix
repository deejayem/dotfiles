{ lib, hostname, ... }:
let
  secretsLib = import ../../../lib/secrets-indexer.nix { inherit lib; };
  inherit (secretsLib) importPrivateIfExists;
in
{
  # This should only contain sensitive information such as email
  # addresses that are not really secrets, but are better kept
  # private. They are encrypted with git-crypt, and are read
  # at build time, and written to the nix store.
  host.private = importPrivateIfExists ../../${hostname}/secrets/private.nix;
}
