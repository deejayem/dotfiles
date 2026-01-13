{ config, lib, ... }:
let
  privateFile = ./private.nix.age;
in
{
  # This should only contain sensitive information such as email
  # addresses that are not really secrets, but are better kept
  # private
  host.private =
    if !(builtins.pathExists privateFile) then
      { }
    else if builtins.extraBuiltins == null then
      throw "extraBuiltins is not available"
    else if !(builtins.extraBuiltins ? readRageForHost) then
      throw "extraBuiltins.readRageForKey is not available"
    else
      builtins.extraBuiltins.readRageForKey "${config.home.homeDirectory}/.ssh/agenix" privateFile;
}
