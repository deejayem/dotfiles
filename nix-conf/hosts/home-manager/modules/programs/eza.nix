{
  ...
}:
{
  programs.eza = {
    enable = true;
    git = true;
    icons = "auto";
    enableBashIntegration = false;
    enableZshIntegration = false;
    enableFishIntegration = false;
    enableIonIntegration = false;
    extraOptions = [
      "--colour=auto"
      "--long"
      "--group-directories-first"
      "--classify"
      "--no-user"
      "--no-time"
      "--no-filesize"
      "--no-permissions"
    ];
  };

  programs.zsh.shellAliases = {
    l = "eza";
    la = "eza -a";
    lg = "eza -G";
    lga = "eza -aG";
    ll = "\\eza --icons --git --colour --long --group-directories-first --classify";
    lla = "ll -a";
    t = "eza --tree";
  };
}
