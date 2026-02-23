{
  pkgs,
  lib,
  ...
}:
let
  bat = lib.getExe pkgs.bat;
  # TODO this can just be lib.getExe pkgs.ea when 26.05 is released
  ea = lib.getExe' pkgs.ea "ea";
in
{
  home.packages = [
    pkgs.ea
  ];

  programs.zsh = {
    shellAliases = {
      els = "ea run linear ls -- -1";
      erg = "ea run grouped rg --";
      fd = "ea run linear fd --";
    };

    siteFunctions = {
      # These functions are called as follows, after using ea (using vip as an example):
      # vip  # edits the first result from ea (roughly equivalent to vi `ea p 1`)
      # vip <n> # edits the nth result from ea (vi `ea p <n>`)
      # vip <n> foo # if the nth result from ea is a directory, edit foo in that directory (vi `ea p <n>`/foo)
      # Will add +<line-number>, where the line number is available
      _vip = ''
        local cmd=(''${=1}) # zsh only, not portable; something like CMD=($(echo $1)) is more portable but is ugly
        local idx=''${2:-1}
        local base_path=$(${ea} p $idx)
        local line=$(${ea} p $idx "{line}")
        local ea_format="'{path}'"

        if [ -z "$base_path" ]; then
          echo "No file path found for index $2"
          return 1
        fi

        if [ $# -gt 2 -a ! -d "$base_path" ]; then
          echo "$base_path is not a directory"
          return 2
        fi

        if [ $# -lt 3 -a $line -ne 1 ]; then
          ea_format+=" +{line}"
        fi

        eval $(${ea} p $idx "$cmd ''${ea_format}$3")
      '';

      vip = ''_vip $EDITOR "$@"'';
      bp = ''_vip ${bat} "$@"'';
      bpp = ''
        # this will be split into an array in _vip
        local CMD="${bat} -p"
        _vip $CMD "$@"
      '';

      ecd = "cd $(${ea} p \${1:-1})";
    };

    initContent = ''
      # Expand e<n><tab> to the corresponding path, or complete with fzf-tab for e<tab>
      _ea_completer () {
        emulate -L zsh
        setopt extendedglob

        if [[ $PREFIX == (#b)e([0-9]##) ]]; then
          local num="$match[1]"
          local expanded
          expanded=$(${ea} p "$num") || return 1
          compadd -U -- "$expanded"
          return 0
        elif [[ $PREFIX == e ]]; then
          local -a nums paths descr
          local line n
          local -i max_width=0

          for line in "''${(@f)$(${ea} list 2>/dev/null)}"; do
            if [[ $line == (#b)\[([0-9]##)\]\ (*) ]]; then
              n="$match[1]"
              paths+=("$match[2]")
              nums+=("$n")
              (( ''${#n} > max_width )) && max_width=''${#n}
            fi
          done

          (( ''${#paths[@]} )) || return 1

          local -i i
          for (( i=1; i<= ''${#paths}; i++ )); do
            descr+=("[$(printf '%0*d' max_width ''${nums[i]})] ''${paths[i]}")
          done

          compadd -U -d descr -a paths
          return 0
        fi

        return 1
      }

      zstyle -g _completers ':completion:*' completer
      _completers=(''${_completers:#_ea_completer})
      zstyle ':completion:*' completer _ea_completer "''${_completers[@]}"
    '';
  };
}
