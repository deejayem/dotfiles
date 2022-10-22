{ config, pkgs, ... }:
{

  imports = [
    ./zsh.nix
  ];

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    bat
    bottom
    broot
    curl
    difftastic
    duf
    du-dust
    elinks
    exa
    fd
    fzf
    git
    gnupg
    gopass
    ispell
    isync
    jq
    lscolors
    lsd
    lynx
    nixfmt
    nix-info
    nix-prefetch-git
    nix-prefetch-github
    pinentry
    pass
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
}

