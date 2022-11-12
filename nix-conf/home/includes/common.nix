{ config, pkgs, ... }:
let
  hcr = pkgs.callPackage ./scripts/hm-changes-report.nix { inherit config pkgs; };
  scr = pkgs.callPackage ./scripts/system-changes-report.nix { inherit config pkgs; };
in
{

  imports = [
    ./zsh.nix
  ];

  home.packages = with pkgs; [
    hcr
    scr

    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    bat
    bat-extras.batdiff
    bat-extras.batgrep
    bat-extras.batman
    bat-extras.batwatch
    #bat-extras.batpipe # wait for 22.11
    bottom
    broot
    curl
    difftastic
    duf
    du-dust
    elinks
    entr
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
    nvd
    pinentry
    pass
    libqalculate
    ripgrep
    rlwrap
    sd
    tealdeer
    ugrep
    vim
    w3m
  ];

  programs.gpg.enable = true;
  programs.nix-index = {
    enable = true;
    enableZshIntegration = false;
    enableBashIntegration = false;
  };
}

