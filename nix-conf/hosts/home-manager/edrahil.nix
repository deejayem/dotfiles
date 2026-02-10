{ ... }:
{
  imports = [
    ./modules/base.nix
    ./modules/programs/irssi.nix
  ];

  home.sessionVariables = {
    TMUX_AUTO_ATTACH = 1;
  };

  home.stateVersion = "22.05";
}
