{ config, pkgs, ... }:
{

  imports = [
    ./zsh.nix
  ];

  home.packages = with pkgs; [
    bat
    bottom
    broot
    curl
    difftastic
    #docker
    #docker-compose
    duf
    du-dust
    elinks
    exa
    fd
    fzf
    git
    gnupg
    gopass
    gopass-jsonapi
    heroku
    isync
    jq
    lscolors
    lsd
    lynx
    mopidy
    mopidy-ytmusic
    mopidy-scrobbler
    mpv
    mu
    neovim
    nix-info
    nix-prefetch-git
    nix-prefetch-github
    #pinentry
    #procs
    libqalculate
    ripgrep
    rlwrap
    sd
    tealdeer
    ugrep
    vim
    w3m
    zenith
  ];

  programs.gpg.enable = true;
  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    #tmuxp.enable = true;
    extraConfig = ''
      set-option -g status-bg '#666666'
      set-option -g status-fg '#aaaaaa'
      set-option -g status-left-length 50
      set-option -g status-right " #(date ''\'+%a, %b %d - %I:%M''\') "
    '';
  };

}
