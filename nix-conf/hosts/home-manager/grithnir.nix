{ ... }:
{
  imports = [
    ./modules/base.nix
    ./modules/orgs/kevel
  ];

  home.stateVersion = "25.05";
}
