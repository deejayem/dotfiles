{ exec, ... }:
{
  # TODO do we need to be able to specify the keyfile?
  # Based on https://github.com/Mic92/sops-nix/issues/624#issuecomment-3070348728
  readSops = file: exec [ "sh" "-c"
    ("nix eval --expr \"builtins.fromJSON(''\$(sops --output-type json -d '" + file + "')'')\"")
  ];
}
