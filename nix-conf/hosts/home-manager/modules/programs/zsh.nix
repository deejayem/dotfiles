{
  pkgs,
  lib,
  ...
}:
let
  gopass = lib.getExe pkgs.gopass;
  tmux = lib.getExe pkgs.tmux;
  tre = lib.getExe pkgs.tre-command;
  tty = lib.getExe' pkgs.coreutils "tty";
  wc = lib.getExe' pkgs.coreutils "wc";
in
{
  home.packages = with pkgs; [ zsh-completions ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion = {
      enable = true;
      highlight = "fg=#808080";
      strategy = [
        "history"
        "completion"
      ];
    };
    defaultKeymap = "emacs";
    history = {
      append = true;
      extended = true;
      ignoreSpace = true;
      save = 300000;
      share = true;
      size = 150000;
    };
    historySubstringSearch = {
      enable = true;
      searchUpKey = if pkgs.stdenv.isDarwin then "^[[A" else "$terminfo[kcuu1]";
      searchDownKey = if pkgs.stdenv.isDarwin then "^[[B" else "$terminfo[kcud1]";
    };

    shellAliases = {
      cp = "cp -iv";
      mv = "mv -iv";
      mkdir = "mkdir -v";

      ".." = "cd ..";
      "..." = "cd ../..";
      "-" = "cd -";

      pp = ''pushbullet push "Pixel" link "''${1}" "''${1}"'';
    };

    setOptions = [
      "INC_APPEND_HISTORY"
      "NO_CLOBBER"
    ];

    localVariables = {
      LESS = "-iRXF";
    };

    siteFunctions = {
      generate = "${gopass} generate -s -p $1 $((RANDOM % 14 + 45))";

      # From omz
      mkcd = ''mkdir -p "$@" && cd ''${@:$#}'';

      tre = ''command ${tre} "$@" -e && source "/tmp/tre_aliases_$USER" 2>/dev/null;'';

      _histfile_watchdog = ''
        local lines
        lines=$(${wc} -l < "$HISTFILE" 2>/dev/null) || return
        lines=''${lines##* }

        if [[ -n $HISTFILE_LAST_GOOD_COUNT && $HISTFILE_LAST_GOOD_COUNT -gt 0 ]]; then
          local drop_pct=$(( 100 - (lines * 100 / HISTFILE_LAST_GOOD_COUNT) ))
          if [[ $drop_pct -gt 10 ]]; then
            local backup="''${HISTFILE}.emergency.$$"
            print -u2 "WARNING: HISTFILE dropped by ''${drop_pct}% ($HISTFILE_LAST_GOOD_COUNT -> $lines lines)"
            print -u2 "Shell PID: $$, TTY: $(${tty}), SHLVL: $SHLVL"
            print -u2 "Last command: $history[$((HISTCMD-1))]"
            fc -W "$backup" 2>/dev/null && print -u2 "In-memory history saved to $backup"
          else
            HISTFILE_LAST_GOOD_COUNT=$lines
          fi
        else
          HISTFILE_LAST_GOOD_COUNT=$lines
        fi

        return 0
      '';
    };

    initContent = lib.mkMerge [
      (lib.mkBefore ''
        [[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return
      '')
      ''
        # Based on prezto tmux plugin
        if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIM" && -z "$INSIDE_EMACS" && "$TERM_PROGRAM" != "vscode" && (-z "$SSH_TTY" || -n "$TMUX_AUTO_ATTACH") ]]; then
          ${tmux} start-server

          if ! ${tmux} has-session 2> /dev/null; then
            ${tmux} new-session -d -s "0" \; set-option -t "0" destroy-unattached off &> /dev/null
          fi

          if [[ -n "$SSH_TTY" ]]; then
            exec ${tmux} -u attach-session
          else
            exec ${tmux} -u attach-session -d
          fi
        fi

        precmd_functions+=(_histfile_watchdog)

        autopair-init

        # make home and end work
        [[ -z "$terminfo[khome]" ]] || bindkey -M emacs "$terminfo[khome]" beginning-of-line
        [[ -z "$terminfo[kend]" ]] || bindkey -M emacs "$terminfo[kend]" end-of-line

        # disable sort when completing `git checkout`
        zstyle ':completion:*:git-checkout:*' sort false
        # set descriptions format to enable group support
        zstyle ':completion:*:descriptions' format '[%d]'
        # Allow tab to expand aliases
        zstyle ':completion:*' completer _expand_alias _complete _ignored
        # set list-colors to enable filename colorizing
        #zstyle ':completion:*' list-colors ''${(s.:.)LS_COLORS}

        [[ ! -f ~/.zsh.local ]] || source ~/.zsh.local
      ''
    ];

    plugins = with pkgs; [
      {
        name = "zsh-autopair";
        src = zsh-autopair;
        file = "share/zsh/zsh-autopair/autopair.zsh";
      }
      {
        name = "zsh-bd";
        src = zsh-bd;
        file = "share/plugins/zsh-bd/bd.plugin.zsh";
      }
      {
        name = "zsh-fast-syntax-highlighting";
        src = zsh-fast-syntax-highlighting;
        file = "share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh";
      }
      {
        name = "zsh-forgit";
        src = zsh-forgit;
        file = "share/zsh/zsh-forgit/forgit.plugin.zsh";
      }
      {
        name = "zsh-edit";
        src = zsh-edit;
        file = "share/zsh/zsh-edit/zsh-edit.plugin.zsh";
      }
    ];
  };
}
