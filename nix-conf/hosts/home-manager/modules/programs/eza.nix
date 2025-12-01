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
}
