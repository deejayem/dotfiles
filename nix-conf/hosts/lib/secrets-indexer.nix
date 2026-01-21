{ lib }:
let

  backends = {
    age = {
      subdir = "age";
      fileExtension = "age";
      mkSecretValue = path: { file = path; };
    };
    sops = {
      subdir = "sops";
      fileExtension = "yml";
      mkSecretValue = path: {
        sopsFile = path;
        key = "value";
      };
    };
  };

  getBackend =
    secretType:
    backends.${secretType}
      or (throw "Unknown secretType '${secretType}'. Expected one of: ${lib.concatStringsSep ", " (lib.attrNames backends)}");

  secretsFromDir =
    {
      dir,
      namePrefix ? "",
      fileExtension,
      mkSecretValue,
    }:
    lib.optionals (builtins.pathExists dir) (
      let
        files = lib.fileset.toList (lib.fileset.fileFilter (f: f.hasExt fileExtension) dir);
        mkSecret =
          path:
          let
            relativePath = lib.removePrefix (toString dir + "/") (toString path);
            secretName = namePrefix + (lib.removeSuffix ".${fileExtension}" relativePath);
          in
          lib.nameValuePair secretName (mkSecretValue path);
      in
      map mkSecret files
    );

  discoverSecrets =
    {
      dirs,
      fileExtension,
      mkSecretValue,
    }:
    let
      pairs = lib.concatMap (d: secretsFromDir (d // { inherit fileExtension mkSecretValue; })) dirs;
    in
    {
      inherit pairs;
      attrs = builtins.listToAttrs pairs;
      hasSecrets = pairs != [ ];
    };

  discoverHomeSecrets =
    {
      secretType,
      baseDir,
      orgsDir,
    }:
    let
      backend = getBackend secretType;

      secretsDir = baseDir + "/${backend.subdir}";
      subpath = "secrets/${backend.subdir}";

      orgDirs = lib.optionals (builtins.pathExists orgsDir) (
        lib.mapAttrsToList
          (org: _: {
            dir = orgsDir + "/${org}/${subpath}";
            namePrefix = "${org}/";
          })
          (lib.filterAttrs (_: t: t == "directory") (builtins.readDir orgsDir))
      );

      dirs = [ { dir = secretsDir; } ] ++ orgDirs;
    in
    discoverSecrets {
      inherit dirs;
      inherit (backend) fileExtension mkSecretValue;
    };

  discoverHostSecrets =
    {
      secretType,
      hostDir,
    }:
    let
      backend = getBackend secretType;
      dir = hostDir + "/${backend.subdir}";
    in
    discoverSecrets {
      dirs = [ { inherit dir; } ];
      inherit (backend) fileExtension mkSecretValue;
    };
in
{
  inherit discoverHomeSecrets discoverHostSecrets;
}
