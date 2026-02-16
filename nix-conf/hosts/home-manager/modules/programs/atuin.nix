{ lib, ... }:
{
  programs.atuin = {
    daemon.enable = true;
    enable = true;
    enableZshIntegration = true;
    flags = [
      "--disable-ctrl-r"
      "--disable-up-arrow"
    ];
    forceOverwriteSettings = true;
    settings = {
      dialect = "uk";
      filter_mode = "host";
      filter_mode_shell_up_key_binding = "session-host";
      inline_height = 30;
      search_mode_shell_up_key_binding = "fulltext";
      style = "full";
      update_check = false;
    };
  };

  programs.zsh.initContent = lib.mkAfter ''
    bindkey '^[r' atuin-search
    bindkey '^[p' atuin-up-search
  '';
}
