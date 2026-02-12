{ lib, ... }:
{
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    flags = [
      "--disable-ctrl-r"
      "--disable-up-arrow"
    ];
  };

  programs.zsh.initContent = lib.mkAfter ''
    bindkey '^[r' atuin-search
  '';
}
