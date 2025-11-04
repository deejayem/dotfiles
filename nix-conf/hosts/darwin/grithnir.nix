{ ... }:
{
  imports = [
    ./base.nix
  ];

  _module.args.username = "djm";

  system.stateVersion = 6;
}
