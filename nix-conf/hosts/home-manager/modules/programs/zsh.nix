{
  pkgs,
  lib,
  ...
}:
let
  gopass = lib.getExe pkgs.gopass;
  jq = lib.getExe pkgs.jq;
  nix = lib.getExe pkgs.nix;
  nixCollectGarbage = lib.getExe' pkgs.nix "nix-collect-garbage";
  sed = lib.getExe pkgs.gnused;
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
      PER_DIRECTORY_HISTORY_TOGGLE = "^\\\\"; # ^\\ is ^#
      HISTORY_START_WITH_GLOBAL = true;
      LESS = "-iRXF";
    };

    siteFunctions = {
      generate = "${gopass} generate -s -p $1 $((RANDOM % 14 + 45))";

      denix = ''
        ${sed} -E 's#/nix/store/[^/]+/bin/([[:alnum:]_.+-]+)#\1#g'
      '';

      # From omz
      mkcd = ''mkdir -p "$@" && cd ''${@:$#}'';

      tre = ''command ${tre} "$@" -e && source "/tmp/tre_aliases_$USER" 2>/dev/null;'';

      gcd = ''
        if [ $# -eq 0 ]; then
          echo "Number of days must be specified" >&2
          return 1
        fi
        if ! [[ $1 =~ '^[0-9]+$' ]]; then
          echo "Number of days must be a number" >&2
          return 2
        fi

        local GC_ARGS
        if [ $1 -eq 0 ]; then
          GC_ARGS=(-d)
        else
          GC_ARGS=(--delete-older-than ''${1}d)
        fi

        local DOAS=$(command -v doas || command -v sudo)

        ${nixCollectGarbage} ''${GC_ARGS[@]}
        if [ -n "$DOAS" ]; then
          $DOAS ${nixCollectGarbage} ''${GC_ARGS[@]}
        fi

        df -h
        date
      '';

      nixos-eval = "${nix} eval --json $NH_FLAKE#nixosConfigurations.$HOST.config.$1 | ${jq}";

      hm-eval = ''${nix} eval --json $NH_FLAKE#homeConfigurations."$USER@$HOST".config.$1 | ${jq}'';

      darwin-eval = "${nix} eval --json $NH_FLAKE#darwinConfigurations.$HOST.config.$1 | ${jq}";

      _histfile_watchdog = ''
        local lines
        lines=$(${wc} -l < "$HISTFILE" 2>/dev/null) || return

        if [[ $lines -lt 1000 && -n $HISTFILE_LAST_GOOD_COUNT && $HISTFILE_LAST_GOOD_COUNT -gt 1000 ]]; then
          local backup="''${HISTFILE}.emergency.$$"

          print -u2 "WARNING: HISTFILE dropped from $HISTFILE_LAST_GOOD_COUNT to $lines lines"
          print -u2 "Shell PID: $$, TTY: $(${tty}), SHLVL: $SHLVL"
          print -u2 "Last command: $history[$((HISTCMD-1))]"

          fc -W "$backup" 2>/dev/null && print -u2 "In-memory history saved to $backup"
        fi

        [[ $lines -gt 1000 ]] && HISTFILE_LAST_GOOD_COUNT=$lines
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
      {
        name = "per-directory-history";
        src = fetchFromGitHub {
          owner = "jimhester";
          repo = "per-directory-history";
          rev = "95f06973e9f2ff0ff75f3cebd0a2ee5485e27834";
          hash = "sha256-EV9QPBndwAWzdOcghDXrIIgP0oagVMOTyXzoyt8tXRo=";
        };
        file = "per-directory-history.zsh";
      }
    ];
  };
}
