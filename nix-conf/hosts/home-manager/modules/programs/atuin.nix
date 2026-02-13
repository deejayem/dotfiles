{ lib, ... }:
{
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    flags = [
      "--disable-ctrl-r"
      "--disable-up-arrow"
    ];
    forceOverwriteSettings = true;
    settings = {
      dialect = "uk";
      update_check = false;
    };
  };

  programs.zsh.initContent = lib.mkAfter ''
    bindkey '^[r' atuin-search
  '';
}
