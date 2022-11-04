{ config, pkgs, ... }:
{
  imports = [
    ./common.nix
  ];

  home.packages = with pkgs; [
    duplicity
    emacs-nox
    irssi
    msmtp
    neomutt
    restic
  ];

  ## TODO programs.tmux.prefix newSession plugins etc
  programs.tmux = {
    enable = true;
    #terminal = "screen-256color";
    #tmuxp.enable = true;
    extraConfig = ''
        unbind C-b
        set -g prefix C-a
        bind-key C-a last-window
        bind-key a send-prefix

        set-window-option -g automatic-rename off
        set-option -g allow-rename off

        set -g bell-action current

        set-option -g lock-command vlock
        set -g lock-after-time 0 # Seconds; 0 = never
        bind L lock-session
        bind l lock-client

        bind-key . source-file ~/.tmux.conf \; display-message "~/.tmux.conf reloaded"

        set -g status-bg black
        set -g status-fg white

        setw -g window-status-current-format "#[fg=red,bold][#[fg=default]#F#I:#W#F#[fg=red,bold]]#[default]"
        setw -g window-status-format "#[fg=green]{#[default]#F#I:#W#F#[fg=green]}#[default]"

        set -g status-left-length 17

        set -g status-interval 1

        set -g status-left "#[fg=yellow]#h#[default]"
        set -g status-right "#[fg=magenta,bold]#(/home/djm/bin/showmail.sh)#[fg=blue]%a%d/%m#[fg=yellow]%H:%M:%S"
    '';
  };

}

