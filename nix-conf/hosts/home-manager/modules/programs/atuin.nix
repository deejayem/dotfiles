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
}
