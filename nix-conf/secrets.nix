let
  hosts = {
    edrahil = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnxKfvsr3HvFyg7iKgJiNX4JHj7lWHa6eXw3zL4d1NS root@edrahil";
    };
  };

  userKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAMhfR/+EKeBA3Avr3I19d7ZzkGbdJboXrDXEEL1Www djm@edrahil";
  userSecrets = [ userKey ];

  hostSecretsAttrs = builtins.foldl' (
    acc: hostname:
    let
      hostConfig = hosts.${hostname};
      hostKeys = [
        hostConfig.hostKey
        userKey
      ];
      hostSecretsDir = ./hosts/nixos/${hostname};

      hostSecrets =
        if builtins.pathExists hostSecretsDir then
          let
            entries = builtins.readDir hostSecretsDir;
            ageFiles = builtins.filter (name: builtins.match ".*\\.age$" name != null) (
              builtins.attrNames entries
            );
          in
          builtins.listToAttrs (
            map (filename: {
              name = "hosts/nixos/${hostname}/${filename}";
              value.publicKeys = hostKeys;
            }) ageFiles
          )
        else
          { };
    in
    acc // hostSecrets
  ) { } (builtins.attrNames hosts);

  homeSecretsPath = "hosts/home-manager/modules/home-secrets/secrets/age";
  homeSecretsDir = ./. + "/${homeSecretsPath}";

  findAgeFiles =
    dir: prefix:
    let
      entries = builtins.readDir dir;
      processEntry =
        name: kind:
        let
          path = if prefix == "" then name else "${prefix}/${name}";
        in
        if kind == "directory" then
          findAgeFiles (dir + "/${name}") path
        else if builtins.match ".*\\.age$" name != null then
          [ path ]
        else
          [ ];
    in
    builtins.concatLists (builtins.attrValues (builtins.mapAttrs processEntry entries));

  homeSecretFiles = findAgeFiles homeSecretsDir "";

  mkSecret = filename: {
    name = "${homeSecretsPath}/${filename}";
    value.publicKeys = userSecrets;
  };

  homeSecretsAttrs = builtins.listToAttrs (map mkSecret homeSecretFiles);
in
homeSecretsAttrs // hostSecretsAttrs
