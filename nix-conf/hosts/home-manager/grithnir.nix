{ ... }:
{
  imports = [
    ./common.nix
    ./modules/orgs/kevel.nix
  ];

  home.stateVersion = "25.05";
}
