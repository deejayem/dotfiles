{ config, pkgs, lib, ... }:

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
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;
    defaultKeymap = "emacs";
    history = {
      size = 100000;
      save = 100000;
    };

    envExtra = ''
      export LSP_USE_PLISTS=true
      export LESS=-iRXF
    '';
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
      pp = "pushbullet push \"Pixel\" link \"\${1}\" \"\${1}\"";
      upgrade_emacs = "cp /Users/djm/.emacs.d/straight/versions/default.el straight-versions-default-`date \"+%Y-%m-%d-%H%M%S\"`.el && emacs --batch -l \"~/.emacs.d/init.el\" -f \"my/upgrade-packages\"";

      # Git log aliases from the omz git plugin
      gl = "git pull";
      glg = "git log --stat";
      glgp = "git log --stat -p";
      glgg = "git log --graph";
      glgga = "git log --graph --decorate --all";
      glgm = "git log --graph --max-count=10";
      glo = "git log --oneline --decorate";
      glol = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset\"";
      glols = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset\" --stat";
      glod = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset\"";
      glods = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset\" --date=short";
      glola = "git log --graph --pretty=\"%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset\" --all";
      glog = "git log --oneline --decorate --graph";
      gloga = "git log --oneline --decorate --graph --all";
    };
         
    initExtra = ''
      # Taken from prezto tmux plugin
      if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIM" && -z "$INSIDE_EMACS" && -z "$SSH_TTY" ]]; then
        tmux start-server

        if ! tmux has-session 2> /dev/null; then
          tmux new-session -d -s "0" \; set-option -t "0" destroy-unattached off &> /dev/null
        fi

        exec tmux -u attach-session -d
      fi

      export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
      export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
      export FZF_ALT_C_COMMAND="rg --hidden --files --sort-files --null | xargs -0 dirname | sort -u"
      if [ -d "~/fzf-git" ]; then
        source ~/fzf-git/functions.sh
        source ~/fzf-git/key-binding.zsh
      fi
      
      autopair-init
      enable-fzf-tab
      bindkey '^[[A' history-substring-search-up
      bindkey '^[[B' history-substring-search-down

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

      function generate () { gopass generate -s -p $1 $((RANDOM % 14 + 45)) }

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
        name = "forgit";
        src = fetchFromGitHub {
          owner = "wfxr";
          repo = "forgit";
          rev = "810735294107ef1b1de66cf1cdfc358fc14049ac";
          sha256 = "11mydkl8psic57qzkzyjlff9wl6inwx9hn3a0vqyfr78pv6vk23y";
        };
        file = "forgit.plugin.zsh";
      }
      {
        name = "zsh-edit";
        src = fetchFromGitHub {
          owner = "marlonrichert";
          repo = "zsh-edit";
          rev = "17b17e5f32fc69349cb9474cf591d5c74e399cdc";
          sha256 = "0nvb7jril7in5b0279z25vmlc0karhagmfsxpjw1rzni5qpyak1s";
        };
        file = "zsh-edit.plugin.zsh";
      }
    ];
  };
}

