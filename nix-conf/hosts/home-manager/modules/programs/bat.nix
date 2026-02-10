{
  pkgs,
  ...
}:
{
  programs.bat = {
    enable = true;
    extraPackages = with pkgs.bat-extras; [
      batdiff
      batgrep
      batman
      batwatch
      batpipe
    ];
    config = {
      style = "full";
      pager = "less -RXF";
      map-syntax = [
        ".ignore:Git Ignore"
        "*.jenkinsfile:Groovy"
      ];
    };
  };

  programs.zsh.shellAliases = {
    cat = "bat -p";
  };
}
