{ exec, ... }:
{
  # Based on https://github.com/Mic92/sops-nix/issues/624#issuecomment-3070348728
  readSopsForKey = key-file: secrets-file: exec [ "sh" "-c"
    ("nix eval --expr \"builtins.fromJSON(''\$(SOPS_AGE_KEY_FILE=" + key-file + " sops --output-type json -d '" + secrets-file + "')'')\"")
  ];

  readSopsForHost = secrets-file: exec [ "sh" "-c"
    ("nix eval --expr \"builtins.fromJSON(''\$(SOPS_AGE_KEY=`doas ssh-to-age -private-key -i /etc/ssh/ssh_host_ed25519_key` sops --output-type json -d '" + secrets-file + "')'')\"")
  ];
}
