{ lib }:
let

  importPrivate =
    file:
    let
      result = builtins.tryEval (import file);
    in
    if result.success then
      result.value
    else
      throw "Failed to import ${toString file}. Perhaps it has not been decrypted.";

  importPrivateIfExists = file: lib.optionalAttrs (builtins.pathExists file) (importPrivate file);

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
      org ? null, # { name, dir } or null
    }:
    let
      backend = getBackend secretType;
      secretsDir = baseDir + "/${backend.subdir}";
      subpath = "secrets/${backend.subdir}";

      dirs = [
        { dir = secretsDir; }
      ]
      ++ lib.optionals (org != null && builtins.pathExists org.dir) [
        {
          dir = org.dir + "/${subpath}";
          namePrefix = "${org.name}/";
        }
      ];
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
  mkOrg =
    orgName: orgsDir:
    if orgName != null then
      {
        name = orgName;
        dir = orgsDir + "/${orgName}";
      }
    else
      null;
in
{
  inherit
    importPrivateIfExists
    discoverHomeSecrets
    discoverHostSecrets
    mkOrg
    ;
}
