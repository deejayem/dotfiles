{
  prefix ? null,
}:
{ config, lib, ... }:

let
  inherit (lib)
    concatStringsSep
    filterAttrs
    hasPrefix
    mapAttrs'
    mapAttrsToList
    nameValuePair
    optionalString
    removePrefix
    replaceStrings
    toUpper
    ;

  secretsPrefix = optionalString (prefix != null) "${prefix}/" + "env/";

  envSecrets = mapAttrs' (
    key: _:
    let
      secretName = removePrefix secretsPrefix key;
      envName = toUpper (replaceStrings [ "-" "/" "." ] [ "_" "_" "_" ] secretName);
    in
    nameValuePair envName key
  ) (filterAttrs (k: _: hasPrefix secretsPrefix k) config.age.secrets);
in
{
  programs.zsh.envExtra = concatStringsSep "\n" (
    mapAttrsToList (envName: secretKey: ''
      if [ -e ${config.age.secrets.${secretKey}.path} ]; then
        export ${envName}=$(<${config.age.secrets.${secretKey}.path})
      fi
    '') envSecrets
  );
}
