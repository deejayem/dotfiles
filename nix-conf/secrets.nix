let
  hosts = {
    edrahil = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnxKfvsr3HvFyg7iKgJiNX4JHj7lWHa6eXw3zL4d1NS root@edrahil";
    };
  };

  userKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAMhfR/+EKeBA3Avr3I19d7ZzkGbdJboXrDXEEL1Www djm@edrahil";
  userSecrets = [ userKey ];

  findAgeFiles =
    dir: prefix:
    let
      entries = builtins.readDir dir;
      processEntry =
        name: kind:
        let
          relPath = if prefix == "" then name else "${prefix}/${name}";
        in
        if kind == "directory" then
          findAgeFiles (dir + "/${name}") relPath
        else if builtins.match ".*\\.age$" name != null then
          [ relPath ]
        else
          [ ];
    in
    builtins.concatLists (builtins.attrValues (builtins.mapAttrs processEntry entries));

  findAgeFilesTopLevel =
    dir:
    let
      entries = builtins.readDir dir;
      names = builtins.attrNames entries;
    in
    builtins.filter (n: builtins.match ".*\\.age$" n != null) names;

  mkSecrets =
    {
      secretsPath,
      secretsDir,
      publicKeys,
      recursive ? true,
    }:
    let
      files =
        if builtins.pathExists secretsDir then
          (if recursive then findAgeFiles secretsDir "" else findAgeFilesTopLevel secretsDir)
        else
          [ ];

      mkSecret = filename: {
        name = "${secretsPath}/${filename}";
        value.publicKeys = publicKeys;
      };
    in
    builtins.listToAttrs (map mkSecret files);

  hostSecretsAttrs = builtins.foldl' (
    acc: hostname:
    let
      hostConfig = hosts.${hostname};
      hostKeys = [
        hostConfig.hostKey
        userKey
      ];
      dir = ./hosts/nixos/${hostname};
    in
    acc
    // (mkSecrets {
      secretsPath = "hosts/nixos/${hostname}";
      secretsDir = dir;
      publicKeys = hostKeys;
      recursive = false;
    })
  ) { } (builtins.attrNames hosts);

  homeSecretsPath = "hosts/home-manager/modules/home-secrets/secrets/age";
  homeSecretsAttrs = mkSecrets {
    secretsPath = homeSecretsPath;
    secretsDir = ./. + "/${homeSecretsPath}";
    publicKeys = userSecrets;
    recursive = true;
  };

  orgsRootPath = "hosts/home-manager/modules/orgs";
  orgsRootDir = ./. + "/${orgsRootPath}";
  orgNames =
    if builtins.pathExists orgsRootDir then
      let
        entries = builtins.readDir orgsRootDir;
      in
      builtins.filter (n: entries.${n} == "directory") (builtins.attrNames entries)
    else
      [ ];

  orgSecretsAttrs = builtins.foldl' (
    acc: org:
    acc
    // (mkSecrets {
      secretsPath = "${orgsRootPath}/${org}/secrets/age";
      secretsDir = orgsRootDir + "/${org}/secrets/age";
      publicKeys = userSecrets;
      recursive = true;
    })
  ) { } orgNames;
in
homeSecretsAttrs // orgSecretsAttrs // hostSecretsAttrs
