# https://github.com/Mic92/sops-nix/issues/624#issuecomment-2382291036
{ exec, ... }: {
  readSops = name: exec [ "sops" "-d" name ];
}

