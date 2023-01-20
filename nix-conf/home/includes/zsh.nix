{ config, pkgs, lib, isDarwin, ... }:
let
    inherit (lib) optionals;
in
{
  home.packages = with pkgs; [
    zsh-completions
  ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
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
    agents = [ "ssh" "gpg"];
    keys = [ "id_rsa" "id_ed25519" "C171251002C200F2" ];
  #  extraFlags = [ "--quiet" "--ignore-missing" ];
  };
  programs.command-not-found.enable = true;
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;
    defaultKeymap = "emacs";
    history = {
      size = 100000;
      save = 100000;
      expireDuplicatesFirst = true;
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
      tree = "exa --tree";

      pp = "pushbullet push \"Pixel\" link \"\${1}\" \"\${1}\"";

      upgrade_emacs = "cp ~/.emacs.d/straight/versions/default.el ~/straight-versions-default-`date \"+%Y-%m-%d-%H%M%S\"`.el && emacs --batch -l \"~/.emacs.d/init.el\" -f \"my/upgrade-packages\"";
      diff_emacs = "difft --color always --context 0 $(ls -d1v ~/straight-versions-default-*.el | tail -1) ~/.emacs.d/straight/versions/default.el | grep '\\[9[12]' | egrep -v '(gnu-elpa-mirror|nongnu-elpa|melpa|emacsmirror-mirror)'";

      nix-up = "git -C ~/dotfiles pull && doas nix-channel --update && doas nixos-rebuild switch && nix-channel --update && home-manager switch && system-changes-report && hm-changes-report && df -h && date";
      _nix-up = "doas nix-channel --update && doas nixos-rebuild switch && nix-channel --update && home-manager switch && system-changes-report && hm-changes-report && df -h && date";
      home-up = "git -C ~/dotfiles pull && nix-channel --update && home-manager switch && hm-changes-report";
      _home-up = "nix-channel --update && home-manager switch && hm-changes-report";
      nix-hammer = "nix shell -f https://github.com/jtojnar/nixpkgs-hammering/archive/master.tar.gz -c nixpkgs-hammer";

      fb = "fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'";

      zz = "z $PWD";

      els = "ea run linear ls -- -1";
      erg = "ea run grouped rg --";
      fd = "ea run linear fd --";

      git-reset-branch = "git fetch && git reset --hard origin/\$(git branch --show-current)";

      # Git log aliases from the omz git plugin
      gl = "git pull";
      glg = "git log --stat";
      glgp = "git log --stat -p";
      glgg = "git log --graph";
      glgga = "git log --graph --decorate --all";
      glgm = "git log --graph --max-count=10";
      glo_ = "git log --oneline --decorate";
      glol = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset\"";
      glols = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset\" --stat";
      glod = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset\"";
      glods = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset\" --date=short";
      glola = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset\" --all";
      glog = "git log --oneline --decorate --graph";
      gloga = "git log --oneline --decorate --graph --all";
    };

    localVariables = {
      PER_DIRECTORY_HISTORY_TOGGLE = "^\\\\"; # ^\\ is ^#
      HISTORY_START_WITH_GLOBAL = true;

      ZSH_AUTOSUGGEST_STRATEGY = ["history" "completion"];

      NVM_AUTO_USE = true;
      NVM_LAZY_LOAD = true;

      LSP_USE_PLISTS = true;
      LESS = "-iRXF";
    };
    initExtraFirst = ''
      [[ $TERM == "tramp" ]] && unsetopt zle && PS1='$ ' && return
    '';
    initExtra = ''
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

      # Keep these in initExtra, rather than localVariables, because the order matters
      export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
      export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
      export FZF_ALT_C_COMMAND='rg --hidden --files --sort-files --null -g "!{.git,node_modules}/*" | xargs -0 dirname | sort -u'
      export FZF_ALT_C_OPTS="--preview 'exa --tree {} | head -200'"
      export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind 'ctrl-t:toggle-preview'"
      export FZF_DEFAULT_OPTS="--bind=ctrl-t:toggle-all --bind=ctrl-j:jump"

      eval "$(batpipe)"
      autopair-init
      enable-fzf-tab
      bindkey '^[[A' history-substring-search-up
      bindkey '^[[B' history-substring-search-down

      # make home and end work
      [[ -z "$terminfo[khome]" ]] || bindkey -M emacs "$terminfo[khome]" beginning-of-line
      [[ -z "$terminfo[kend]" ]] || bindkey -M emacs "$terminfo[kend]" end-of-line

      # disable sort when completing `git checkout`
      zstyle ''\':completion:*:git-checkout:*''\' sort false
      # set descriptions format to enable group support
      zstyle ''\':completion:*:descriptions''\' format ''\'[%d]''\'
      # Allow tab to expand aliases
      zstyle ':completion:*' completer _expand_alias _complete _ignored
      # set list-colors to enable filename colorizing
      #zstyle ''\':completion:*''\' list-colors ''${(s.:.)LS_COLORS}
      # preview directory''\'s content with exa when completing cd
      zstyle ''\':fzf-tab:complete:cd:*''\' fzf-preview ''\'exa -1 --color=always ''$realpath''\'
      # switch group using `,` and `.`
      zstyle ''\':fzf-tab:*''\' switch-group ''\',''\' ''\'.''\'

      set -o noclobber append_history share_history

      # disable flow control (so that fzf-git.sh's ^g^s can work)
      stty -ixon

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

      ..() {
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

        DOAS=$(command -v doas)
        if  [ $1 -eq 0 ] ; then
          $DOAS nix-collect-garbage -d
        else
          $DOAS nix-collect-garbage --delete-older-than ''${1}d
        fi
        df -h
      }

      function checkout-pr () {
        git fetch ''${2:-upstream} pull/''${1}/head:pr-''${1}
        git switch pr-''${1}
      }

      [[ ! -f ~/.zsh.local ]] || source ~/.zsh.local

      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
    '';

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
        name = "zsh-history-substring-search";
        src = zsh-history-substring-search;
        file = "share/zsh-history-substring-search/zsh-history-substring-search.zsh";
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
        src = fetchFromGitHub {
          owner = "junegunn";
          repo = "fzf-git.sh";
          rev = "9190e1bf7273d85f435fa759a5c3b20e588e9f7e";
          sha256 = "2CGjk1oTXip+eAJMuOk/X3e2KTwfwzcKTcGToA2xPd4=";
        };
        file = "fzf-git.sh";
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
    ]
    ++ optionals stdenv.isDarwin [
      {
        name = "zsh-nvm";
        src = fetchFromGitHub {
          owner = "lukechilds";
          repo = "zsh-nvm";
          rev = "23067bd9bb6eb6f4737a3ea90cb0cb5e85f61ba2";
          sha256 = "Zwdi7bezMFKaIKYwsSftu3mJSFvadEWmY2hYnU1Kpu4=";
        };
        file = "zsh-nvm.plugin.zsh";
      }
    ];
  };
}

