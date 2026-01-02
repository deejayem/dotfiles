{ exec, ... }:
{
  # Based on https://github.com/Mic92/sops-nix/issues/624#issuecomment-3070348728
  readSopsForKey = keyFile: secretsFile: exec [ "sh" "-c"
    ("nix eval --expr \"builtins.fromJSON(''\$(SOPS_AGE_KEY_FILE=" + keyFile + " sops --output-type json -d '" + secretsFile + "')'')\"")
  ];

  readSopsForHost = secretsFile: exec [ "sh" "-c"
    ("nix eval --expr \"builtins.fromJSON(''\$(SOPS_AGE_KEY=`doas ssh-to-age -private-key -i /etc/ssh/ssh_host_ed25519_key` sops --output-type json -d '" + secretsFile + "')'')\"")
  ];

  readRageForHost = nixAgeFile:
    exec [ "doas" "rage" "-d" "-i" "/etc/ssh/ssh_host_ed25519_key" nixAgeFile ];

  readRageForKey = keyFile: nixAgeFile:
    exec [ "rage" "-d" "-i" keyFile nixAgeFile ];
}
