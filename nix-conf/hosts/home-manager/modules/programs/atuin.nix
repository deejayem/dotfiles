{ lib, pkgs, ... }:
let
  atuin = lib.getExe pkgs.atuin;
  date = lib.getExe' pkgs.coreutils "date";
in
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
      enter_accept = true;
      filter_mode = "host";
      filter_mode_shell_up_key_binding = "session-host";
      inline_height = 30;
      search_mode_shell_up_key_binding = "fulltext";
      style = "full";
      update_check = false;
    };
  };

  programs.zsh.siteFunctions.atuin-to-zsh-extended-history = ''
    ${atuin} history list --format '{time}\t{command}' -r true --print0 | while IFS= read -r -d $'\0' line; do
      time=''${line%%$'\t'*}
      command=''${line#*$'\t'}

      epoch=$(${date} -d "$time" +%s 2>/dev/null) || continue

      command=''${command//$'\n'/$'\\\n'}

      print -r -- ": ''${epoch}:0;''${command}"
    done
  '';

  programs.zsh.initContent = lib.mkAfter ''
    bindkey '^[r' atuin-search
    bindkey '^[p' atuin-up-search
  '';
}
