{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib) optionalAttrs optionals;
  show_file_or_dir_preview = "if [ -d {} ]; then eza --tree --color=always {} | head -200; else bat -n --color=always --line-range :500 {}; fi";
in
{
  home.packages = with pkgs; [ zsh-completions ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    changeDirWidgetCommand = "fd --type=d --hidden --strip-cwd-prefix --exclude .git --exclude node_modules"; # FZF_ALT_C_COMMAND
    changeDirWidgetOptions = [ "--preview 'eza --tree --color=always {} | head -200'" ]; # FZF_ALT_C_OPTS
    defaultCommand = "fd --hidden --strip-cwd-prefix --exclude .git --exclude node_modules"; # FZF_DEFAULT_COMMAND
    defaultOptions = [
      "--bind=ctrl-t:toggle-all"
      "--bind=ctrl-j:jump"
    ]; # FZF_DEFAULT_OPTS
    fileWidgetCommand = config.programs.fzf.defaultCommand; # FZF_CTRL_T_COMMAND
    fileWidgetOptions = [ "--preview '${show_file_or_dir_preview}'" ]; # FZF_CTRL_T_OPTS
    historyWidgetOptions = [
      "--preview 'echo {}'"
      "--preview-window down:3:hidden:wrap"
      "--bind 'ctrl-t:toggle-preview'"
    ]; # FZF_CTRL_R_OPTS
  };
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  programs.keychain = {
    enable = lib.mkIf pkgs.stdenv.isLinux true;
    agents = [
      "ssh"
      "gpg"
    ];
    keys = [
      "id_rsa"
      "id_ed25519"
      "C171251002C200F2"
    ];
    #  extraFlags = [ "--quiet" "--ignore-missing" ];
  };
  programs.command-not-found.enable = true;
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    # TODO: put strategy here after 24.11 is released
    autosuggestion = {
      enable = true;
      highlight = "fg=#808080";
    };
    defaultKeymap = "emacs";
    history = {
      expireDuplicatesFirst = true;
      ignoreSpace = true;
      save = 100000;
      share = true;
      size = 100000;
    };
    historySubstringSearch = {
      enable = true;
      searchUpKey = "$terminfo[kcuu1]";
      searchDownKey = "$terminfo[kcud1]";
    };

    profileExtra = ''
      [[ -f ~/.nix-profile/etc/profile.d/nix.sh ]] && . ~/.nix-profile/etc/profile.d/nix.sh
      path=(~/bin
            ~/.local/bin
            ~/go/bin/
            $path)
    '';
    shellAliases = {
      cp = "cp -iv";
      mv = "mv -iv";
      mkdir = "mkdir -v";
      cat = "bat -p";
      l = "eza";
      la = "eza -a";
      lg = "eza -G";
      lga = "eza -aG";
      ll = "\\eza --icons --git --colour --long --group-directories-first --classify";
      lla = "ll -a";
      t = "eza --tree";

      pp = ''pushbullet push "Pixel" link "''${1}" "''${1}"'';

      upgrade_emacs = ''cp ~/.emacs.d/straight/versions/default.el ~/straight-versions-default-`date "+%Y-%m-%d-%H%M%S"`.el && emacs --batch -l "~/.emacs.d/init.el" -f "my/upgrade-packages"'';
      diff_emacs = "difft --color always --context 0 $(ls -d1v ~/straight-versions-default-*.el | tail -1) ~/.emacs.d/straight/versions/default.el | grep '\\[9[12]' | egrep -v '(gnu-elpa-mirror|nongnu-elpa|melpa|emacsmirror-mirror)'";

      nix-up = "git -C ~/dotfiles pull && doas nix-channel --update && doas nixos-rebuild switch && nix-channel --update && home-manager switch && system-changes-report && hm-changes-report && df -h && date";
      _nix-up = "doas nix-channel --update && doas nixos-rebuild switch && nix-channel --update && home-manager switch && system-changes-report && hm-changes-report && df -h && date";
      home-up = "git -C ~/dotfiles pull && nix-channel --update && home-manager switch && hm-changes-report";
      _home-up = "nix-channel --update && home-manager switch && hm-changes-report";
      nix-hammer = "nix shell -f https://github.com/jtojnar/nixpkgs-hammering/archive/master.tar.gz -c nixpkgs-hammer";

      fb = "fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'";

      # Restrict matches to subdirs of the current one (https://github.com/skywind3000/z.lua/blob/ef9a49d73d2b4f262c6fbb23262253dcda7c19a7/README.md#tips)
      zz = "z $PWD";

      els = "ea run linear ls -- -1";
      erg = "ea run grouped rg --";
      fd = "ea run linear fd --";

      git-reset-branch = "git fetch && git reset --hard origin/$(git branch --show-current)";

      # Git log aliases from the omz git plugin
      gl = "git pull";
      glg = "git log --stat";
      glgp = "git log --stat -p";
      glgg = "git log --graph";
      glgga = "git log --graph --decorate --all";
      glgm = "git log --graph --max-count=10";
      glo_ = "git log --oneline --decorate";
      glol = ''git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset"'';
      glols = ''git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset" --stat'';
      glod = ''git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset"'';
      glods = ''git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset" --date=short'';
      glola = ''git log --graph --pretty="%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset" --all'';
      glog = "git log --oneline --decorate --graph";
      gloga = "git log --oneline --decorate --graph --all";
    } // optionalAttrs pkgs.stdenv.isDarwin { oemacs = "open -a /Applications/Emacs.app"; };

    localVariables = {
      PER_DIRECTORY_HISTORY_TOGGLE = "^\\\\"; # ^\\ is ^#
      HISTORY_START_WITH_GLOBAL = true;

      # TODO move to programs.zsh.autosuggestion.strategy when 24.11 is released
      ZSH_AUTOSUGGEST_STRATEGY = [
        "history"
        "completion"
      ];

      LESS = "-iRXF";
    };
    initContent = lib.mkMerge [
      (lib.mkBefore ''
        [[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return
      '')
      ''
        # Based on prezto tmux plugin
        if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIM" && -z "$INSIDE_EMACS" && (-z "$SSH_TTY" || -n "$TMUX_AUTO_ATTACH") ]]; then
          tmux start-server

          if ! tmux has-session 2> /dev/null; then
            tmux new-session -d -s "0" \; set-option -t "0" destroy-unattached off &> /dev/null
          fi

          if [[ -n "$SSH_TTY" ]]; then
            exec tmux -u attach-session
          else
            exec tmux -u attach-session -d
          fi
        fi

        autopair-init
        enable-fzf-tab

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
        # preview directory's content with eza when completing cd
        zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
        # switch group using `,` and `.`
        zstyle ':fzf-tab:*' switch-group ',' '.'

        # functions modified from https://www.josean.com/posts/7-amazing-cli-tools
        _fzf_compgen_path() {
          fd --hidden --exclude .git --exclude node_modules . "$1"
        }
        _fzf_compgen_dir() {
          fd --type=d --hidden --exclude .git --exclude node_modules . "$1"
        }
        _fzf_comprun() {
          local command=$1
          shift

          case "$command" in
            cd)           fzf --preview 'eza --tree --color=always {} | head -200' "$@" ;;
            export|unset) fzf --preview "eval 'echo $'{}"         "$@" ;;
            ssh)          fzf --preview 'dig {}'                   "$@" ;;
            *)            fzf --preview "${show_file_or_dir_preview}" "$@" ;;
          esac
        }

        set -o noclobber

        # disable flow control (so that fzf-git.sh's ^g^s can work)
        stty -ixon

        # These functions are called as follows, after using ea (using vip as an example):
        # vip  # edits the first result from ea (roughly equivalent to vi `ea p 1`)
        # vip <n> # edits the nth result from ea (vi `ea p <n>`)
        # vip <n> foo # if the nth result from ea is a directory, edit foo in that directory (vi `ea p <n>`/foo)
        # Will add +<line-number>, where the line number is available
        function _vip () {
          local cmd=(''${=1}) # zsh only, not portable; something like CMD=($(echo $1)) is more portable but is ugly
          local idx=''${2:-1}
          local base_path=$(ea p $idx)
          local line=$(ea p $idx "{line}")
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

          eval $(ea p $idx "$cmd ''${ea_format}$3")
        }

        function vip () {
          _vip $EDITOR ''${@}
        }
        function bp () {
          _vip bat ''${@}
        }
        function bpp () {
          # this will be split into an array in _vip
          CMD="bat -p"
          _vip $CMD ''${@}
        }

        function generate () { gopass generate -s -p $1 $((RANDOM % 14 + 45)) }
        function fcd { cd $(fd -L --max-depth=''${1:-4} --type=d 2>/dev/null | fzf-tmux) }

        fif() {
          if [ ! "$#" -gt 0  ]; then
            echo "usage: fif <SEARCH_TERM>"
            return 1;
          fi
          rg --files-with-matches --no-messages "$1" | fzf $FZF_PREVIEW_WINDOW --preview "rg --ignore-case --pretty --context 10 '$1' {}"
        }

        fe() {
          IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
          [[ -n "$files" ]] && ''${EDITOR:-vim} "''${files[@]}"
        }

        # TODO is there a way to do this in shellAliases
        alias ..="cd .."
        alias ...="cd ../.."
        alias -- -="cd -"

        .,() {
          local declare dirs=()
          get_parent_dirs() {
            if [[ -d "''${1}" ]]; then dirs+=("$1"); else return; fi
            if [[ "''${1}" == '/' ]]; then
              for _dir in "''${dirs[@]}"; do echo $_dir; done
            else
              get_parent_dirs $(dirname "$1")
            fi
          }
          local DIR=$(get_parent_dirs $(realpath "$PWD/..") | fzf-tmux)
          cd "$DIR"
        }

        # From omz
        function mkcd () {
          mkdir -p $@ && cd ''${@:$#}
        }

        tre () { command tre "$@" -e && source "/tmp/tre_aliases_$USER" 2>/dev/null; }

        function gcd () {
          if [ $# -eq 0 ] ; then
            echo "Number of days must be specified" >&2
            return 1
          fi
          if ! [[ $1 =~ '^[0-9]+$' ]] ; then
            echo "Number of days must be a number" >&2
            return 2
          fi

          if [ $1 -eq 0 ] ; then
           GC_ARGS=(-d)
          else
            GC_ARGS=(--delete-older-than ''${1}d)
          fi

          DOAS=$(command -v doas)

          # Run as the current user (as well as root) to clean up hm generations
          nix-collect-garbage ''${GC_ARGS[@]}
          if [ -n $DOAS ] ; then
            $DOAS nix-collect-garbage ''${GC_ARGS[@]}
          fi

          df -h
          date
        }

        function checkout-pr () {
          git fetch ''${2:-upstream} pull/''${1}/head:pr-''${1}
          git switch pr-''${1}
        }

        [[ ! -f ~/.zsh.local ]] || source ~/.zsh.local

        [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
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
        file = "share/zsh-bd/bd.zsh";
      }
      {
        name = "zsh-fzf-tab";
        src = zsh-fzf-tab;
        file = "share/fzf-tab/fzf-tab.zsh";
      }
      {
        name = "zsh-fast-syntax-highlighting";
        src = zsh-fast-syntax-highlighting;
        file = "share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh";
      }
      {
        name = "zsh-powerlevel10k";
        src = zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
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
        name = "fzf-git.sh";
        src = fzf-git-sh;
        file = "share/fzf-git-sh/fzf-git.sh";
      }
      {
        name = "per-directory-history";
        src = fetchFromGitHub {
          owner = "jimhester";
          repo = "per-directory-history";
          rev = "0687bbfd736da566472a6d67c2b45c501b73d405";
          sha256 = "7Z0qaDhgopKt9BDKSqdziw9jsVgiLLafs30wPPbz+oo=";
        };
        file = "per-directory-history.zsh";
      }
    ];
  };
}
