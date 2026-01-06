let
  edrahilHostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnxKfvsr3HvFyg7iKgJiNX4JHj7lWHa6eXw3zL4d1NS root@edrahil";
  userKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAMhfR/+EKeBA3Avr3I19d7ZzkGbdJboXrDXEEL1Www djm@edrahil";

  edrahilSecrets = [
    edrahilHostKey
    userKey
  ];
  userSecrets = [ userKey ];
in
{
  "hosts/nixos/edrahil/restic.age".publicKeys = edrahilSecrets;
}
