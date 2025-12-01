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
}
