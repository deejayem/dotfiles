{ config, pkgs, ... }:
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
    #tmuxp.enable = true;
    extraConfig = ''
      set-option -g status-bg '#666666'
      set-option -g status-fg '#aaaaaa'
      set-option -g status-left-length 50
      set-option -g status-right " %a, %b %d - %H:%M "
    '';
  };

}

