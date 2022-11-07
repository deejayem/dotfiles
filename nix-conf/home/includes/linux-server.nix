{ config, pkgs, ... }:
{
  imports = [
    ./common.nix
  ];

  home.packages = with pkgs; [
    emacs-nox
    irssi
    msmtp
    neomutt
    restic
  ];

  services.gpg-agent = {
    enable = true;
    pinentryFlavor = "curses";
  };

  ## TODO newSession plugins etc
  programs.tmux = {
    enable = true;
    #terminal = "screen-256color";
    prefix = "C-a";
    #tmuxp.enable = true;
    extraConfig = ''
        setw -g window-status-current-format "#[fg=red,bold][#[fg=default]#F#I:#W#F#[fg=red,bold]]#[default]"
        setw -g window-status-format "#[fg=green]{#[default]#F#I:#W#F#[fg=green]}#[default]"

        set -g status-left-length 17

        set -g status-interval 1

        set -g status-left "#[fg=yellow]#h#[default]"
        set -g status-right "#[fg=magenta,bold]#(/home/djm/bin/showmail.sh)#[fg=blue]%a%d/%m#[fg=yellow]%H:%M:%S"
    '';
  };

}

