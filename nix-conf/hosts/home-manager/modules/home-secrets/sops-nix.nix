{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  sopsDir = ./secrets-sops;
  sopsDirExists = builtins.pathExists sopsDir;

  sopsFiles =
    if sopsDirExists then
      lib.fileset.toList (lib.fileset.fileFilter (f: f.hasExt "yml") sopsDir)
    else
      [ ];

  mkSopsSecret =
    path:
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
  imports = [ inputs.sops-nix.homeManagerModules.sops ];

  config = lib.mkIf sopsDirExists {
    sops = {
      age.sshKeyPaths = [ "${config.home.homeDirectory}/.ssh/agenix" ];
      secrets = builtins.listToAttrs (map mkSopsSecret sopsFiles);
    };

    home.packages = [ pkgs.sops ];
  };
}
