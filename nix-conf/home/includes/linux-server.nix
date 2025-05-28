{ config, pkgs, ... }:
{
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    emacs-nox
    irssi
    libtree
    msmtp
    pinentry
    restic
    sword
    yt-dlp
  ];

  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry-curses;
    defaultCacheTtl = 34560000;
    maxCacheTtl = 34560000;
  };

  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    prefix = "C-a";
    #tmuxp.enable = true;
    extraConfig = ''
      unbind-key R
      bind-key R run-shell ' \
        tmux source-file ~/.config/tmux/tmux.conf > /dev/null; \
        tmux display-message "Sourced .config/tmux/tmux.conf!"'
      bind-key C-a last-window
      bind-key a send-prefix

      set -g status-bg black
      set -g status-fg white

      setw -g window-status-current-format "#[fg=red,bold][#[fg=default]#F#I:#W#F#[fg=red,bold]]#[default]"
      setw -g window-status-format "#[fg=green]{#[default]#F#I:#W#F#[fg=green]}#[default]"

      set -g status-left-length 17

      set -g status-interval 1

      set -g status-left "#[fg=yellow]#h#[default]"
      set -g status-right "#[fg=blue]%a%d/%m#[fg=yellow]%H:%M:%S"
    '';
  };

  programs.vim.packageConfigurable = pkgs.vim;
}
