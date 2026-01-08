let
  # based on lib.filterAttrs
  filterAttrs =
    pred: set:
    builtins.removeAttrs set (builtins.filter (name: !pred name set.${name}) (builtins.attrNames set));

  edrahilHostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnxKfvsr3HvFyg7iKgJiNX4JHj7lWHa6eXw3zL4d1NS root@edrahil";
  userKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAMhfR/+EKeBA3Avr3I19d7ZzkGbdJboXrDXEEL1Www djm@edrahil";

  edrahilSecrets = [
    edrahilHostKey
    userKey
  ];
  userSecrets = [ userKey ];

  userSecretsPath = "hosts/home-manager/modules/home-secrets/secrets";
  userSecretsDir = ./. + "/${userSecretsPath}";

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

  userSecretFiles = findAgeFiles userSecretsDir "";

  mkSecret = filename: {
    name = "${userSecretsPath}/${filename}";
    value.publicKeys = userSecrets;
  };

  userSecretsAttrs = builtins.listToAttrs (map mkSecret userSecretFiles);
in
userSecretsAttrs
// {
  "hosts/nixos/edrahil/restic.age".publicKeys = edrahilSecrets;
}
