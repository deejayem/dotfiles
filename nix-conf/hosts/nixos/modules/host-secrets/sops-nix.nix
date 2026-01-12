{
  lib,
  pkgs,
  inputs,
  hostname,
  ...
}:
let
  sopsDir = ../../${hostname}/secrets/sops;
  sopsDirExists = builtins.pathExists sopsDir;

  sopsFiles =
    if sopsDirExists then
      lib.fileset.toList (lib.fileset.fileFilter (f: f.hasExt "yml") sopsDir)
    else
      [ ];

  mkSopsSecret = path:
    let
      relativePath = lib.removePrefix (toString sopsDir + "/") (toString path);
      secretName = lib.removeSuffix ".yml" relativePath;
    in
    lib.nameValuePair secretName {
      sopsFile = path;
      key = "value";
    };
in
{
  imports = [ inputs.sops-nix.nixosModules.sops ];

  config = lib.mkIf sopsDirExists {
    sops.secrets = builtins.listToAttrs (map mkSopsSecret sopsFiles);
  };
}
