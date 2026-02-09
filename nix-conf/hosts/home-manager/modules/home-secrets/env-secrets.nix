{ config, lib, ... }:

let
  inherit (lib)
    concatStringsSep
    filterAttrs
    hasPrefix
    mapAttrs'
    mapAttrsToList
    nameValuePair
    removePrefix
    replaceStrings
    toUpper
    ;

  org = config.host.org;

  globalPrefix = "env/";
  orgPrefix = lib.optionalString (org != null) "${org}/env/";

  mkEnvPair =
    prefix: key: _:
    let
      secretName = removePrefix prefix key;
      envName = toUpper (replaceStrings [ "-" "/" "." ] [ "_" "_" "_" ] secretName);
    in
    nameValuePair envName key;

  globalSecrets = mapAttrs' (mkEnvPair globalPrefix) (
    filterAttrs (k: _: hasPrefix globalPrefix k) config.age.secrets
  );
  orgSecrets = lib.optionalAttrs (org != null) (
    mapAttrs' (mkEnvPair orgPrefix) (filterAttrs (k: _: hasPrefix orgPrefix k) config.age.secrets)
  );

  envSecrets = globalSecrets // orgSecrets;
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
