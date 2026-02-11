{ ... }:
{
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    flags = [
      "--disable-ctrl-r"
      "--disable-up-arrow"
    ];
  };

  programs.zsh.initContent = ''
    bindkey '^[r' atuin-search
  '';
}
