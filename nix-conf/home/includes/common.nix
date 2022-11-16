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
    file
    fzf
    git
    gnupg
    gopass
    inetutils
    ispell
    isync
    jq
    libqalculate
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
    ripgrep
    rlwrap
    sd
    tealdeer
    ugrep
    uni
    w3m
  ];

  programs.gpg.enable = true;
  programs.nix-index = {
    enable = true;
    enableZshIntegration = false;
    enableBashIntegration = false;
  };

  programs.vim = {
    enable = true;
    extraConfig = ''
      colorscheme molokai
      " highlight doesn't work properly without this
      syntax enable
      highlight Normal ctermfg=white ctermbg=black
      set hlsearch
      set showmatch
    '';
    plugins = [ pkgs.vimPlugins.sensible
                pkgs.vimPlugins.auto-pairs
                pkgs.vimPlugins.awesome-vim-colorschemes
                pkgs.vimPlugins.ctrlp
                pkgs.vimPlugins.inkpot
                pkgs.vimPlugins.surround
                pkgs.vimPlugins.vim-lastplace
                pkgs.vimPlugins.vim-pasta
                pkgs.vimPlugins.vim-sleuth
    ];
    settings = {
      background = "dark";
      copyindent = true;
      expandtab = true;
      ignorecase = true;
      number = true;
      shiftwidth = 4;
      smartcase = true;
      tabstop = 4;
    };
  };

  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host *
        AddKeysToAgent yes
        IgnoreUnknown UseKeychain
        UseKeychain yes
    '';
    includes = [ "~/.ssh/config_local" ];
    matchBlocks = {
      "djm.ovh" = {
        hostname = "v.djm.ovh";
        port = 2222;
      };
      "devio" = {
         hostname = "devio.us";
         user = "deejayem";
         port = 2222;
      };
      "sdf" = {
        hostname = "sdf.org";
        user = "deejayem";
      };
      "sdfeu" = {
        hostname = "sdf-eu.org";
        user = "deejayem";
      };
      "grex" = {
        hostname = "grex.org";
        user = "deejayem";
      };
      "blinkenshell" = {
         hostname = "ssh.blinkenshell.org";
         port = 2222;
      };
      "hashbang" = {
        hostname = "de1.hashbang.sh";
      };
      "o1" = {
        hostname = "130.162.163.108";
      };
      "o2" = {
        hostname = "143.47.239.39";
      };
      "tilde.institute" = {
        hostname = "tilde.institute";
      };
      "tilde.team" = {
        hostname = "tilde.team";
      };
      "ctrl-c.club" = {
        hostname = "ctrl-c.club";
      };
      "github.com" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_ed25519";
        identitiesOnly = true;
      };
    };
  };
}

