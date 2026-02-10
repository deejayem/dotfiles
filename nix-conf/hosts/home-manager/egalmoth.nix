{ ... }:
{
  imports = [
    ./modules/base.nix
    ./modules/programs/sway.nix
    ./modules/programs/ghostty.nix
  ];

  home.stateVersion = "21.11";
}
