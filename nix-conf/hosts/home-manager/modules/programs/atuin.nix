{ lib, ... }:
{
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    flags = [
      "--disable-ctrl-r"
      "--disable-up-arrow"
    ];
  };

  programs.zsh.initContent = lib.mkAfter ''
    # per-hirectory-history is causing preexec to pass the command as $2
    # instead of $1, leaving $1 empty
    if (( ''${+functions[_atuin_preexec]} )); then
      functions[_atuin_preexec_orig]=$functions[_atuin_preexec]
      _atuin_preexec() {
        local cmd="$1"
        [[ -z "$cmd" ]] && cmd="$2"
        _atuin_preexec_orig "$cmd"
      }
    fi

    bindkey '^[r' atuin-search
  '';
}
