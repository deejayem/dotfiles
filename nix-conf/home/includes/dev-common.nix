{ config, pkgs, lib, ... }:
{

  imports = [
    ./common.nix
    ./clojure.nix
  ];

  home.packages = with pkgs; [
    docker
    docker-compose
    gopass-jsonapi
    mpv
    mu
    neovim
  ];

  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    prefix = "C-x";
    extraConfig = ''
     tmux bind-key R run-shell ' \
       tmux source-file ~/.config/tmux/tmux.conf > /dev/null; \
       tmux display-message "Sourced .config/tmux/tmux.conf!"'
      bind-key £ split-window -h
      set-option -g status-bg '#666666'
      set-option -g status-fg '#aaaaaa'
      set-option -g status-left-length 50
      set-option -g status-right " %a, %b %d - %H:%M "
      ${lib.optionalString pkgs.stdenv.isLinux ''
        bind-key -T copy-mode y send-keys -X copy-pipe-and-cancel "xsel -i -p && xsel -o -p | xsel -i -b"
        bind-key C-y run "xsel -o | tmux load-buffer - ; tmux paste-buffer"
      ''}
    '';
  };

}

