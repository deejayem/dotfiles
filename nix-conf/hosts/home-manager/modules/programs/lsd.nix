{
  ...
}:
{
  programs.lsd = {
    enable = true;
    enableZshIntegration = false; # don't set aliases
    settings = {
      indicators = true;
      #layout = "oneline";
      sorting.dir-grouping = "first";
      blocks = [
        "git"
        "permission"
        "user"
        "group"
        "size"
        "date"
        "name"
      ];
    };
  };

  programs.zsh.shellAliases = {
    p = "lsd";
    pa = "lsd -a";
    pll = "lsd -l";
    pla = "lsd -la";
  };
}
