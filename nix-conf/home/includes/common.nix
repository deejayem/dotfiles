{ config, lib, pkgs, ... }:
let
  hcr = pkgs.callPackage ./scripts/hm-changes-report.nix { inherit config pkgs; };
  scr = pkgs.callPackage ./scripts/system-changes-report.nix { inherit config pkgs; };
  email = builtins.readFile "${config.home.homeDirectory}/email.txt";
  unstable = import <unstable> { };
in
{
  imports = [
    ./zsh.nix
  ];

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "aspell-dict-en-science"
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
    bat-extras.batpipe
    bottom
    broot
    curl
    diff-so-fancy
    difftastic
    duf
    du-dust
    ea
    elinks
    entr
    eza
    fd
    file
    fzf
    git
    gnupg
    gopass
    htop
    inetutils
    ispell
    isync
    jq
    libqalculate
    lscolors
    lsd
    lynx
    mercurial
    nixfmt-classic
    nix-info
    nix-prefetch-git
    nix-prefetch-github
    nixpkgs-review
    nvd
    pass
    ripgrep
    rlwrap
    sd
    tealdeer
    tre-command
    ugrep
    uni
    w3m
    zstd
    unstable.wcurl
  ];

  nix = {
    package = pkgs.nix;
    settings = {
      extra-experimental-features = [ "nix-command" "flakes" ];
    };
  };

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
      set pastetoggle=<F2>
      nmap <silent> <F3> :silent nohlsearch<CR>
      imap <silent> <F3> <C-o>:silent nohlsearch<CR>
    '';
    plugins = [ pkgs.vimPlugins.sensible
                pkgs.vimPlugins.auto-pairs
                pkgs.vimPlugins.ctrlp
                pkgs.vimPlugins.editorconfig-vim
                pkgs.vimPlugins.inkpot
                pkgs.vimPlugins.molokai
                pkgs.vimPlugins.surround
                pkgs.vimPlugins.vim-lastplace
                pkgs.vimPlugins.vim-nix
                pkgs.vimPlugins.vim-pasta
                pkgs.vimPlugins.vim-repeat
                pkgs.vimPlugins.vim-sexp-mappings-for-regular-people
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
        User djm
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
        hostname = "152.67.142.10";
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

  programs.git = {
    enable = true;
    userName = "David Morgan";
    userEmail = email;
    aliases = {
      # difftastic
      logt = "!sh -c 'GIT_EXTERNAL_DIFF=\"difft --background=dark\" git log -p --ext-diff'";
      showt = "!show() { GIT_EXTERNAL_DIFF=difft git show \${1} --ext-diff; }; show";
      difft = "difftool";
      # "raw" output
      rlog = "!git -c delta.raw=true -c core.pager=${pkgs.less}/bin/less log"; # usually used with -p
      rshow = "!git -c delta.raw=true -c core.pager=${pkgs.less}/bin/less show";
      rdiff = "!git -c delta.raw=true -c core.pager=${pkgs.less}/bin/less diff";
      #  copiable output (without line numbers or +/- indicators)
      clog = "!git -c delta.line-numbers=false log"; # usually used with -p
      cshow = "!git -c delta.line-numbers=false show";
      cdiff = "!git -c delta.line-numbers=false diff";
      # diff-so-fancy
      flog = "!git -c core.pager=\"diff-so-fancy | less\" log"; # usually used with -p
      fshow = "!git -c core.pager=\"diff-so-fancy | less\" show";
      fdiff = "!git -c core.pager=\"diff-so-fancy | less\" diff";

      upstream = "!git push -u origin HEAD";
      update-master = "!git fetch origin master:master";
      update-main = "!git fetch origin main:main";
    };
    attributes = [
      "*.el diff=elisp"
      "*.clj diff=clojure"
    ];
    extraConfig = {
      core.editor = "vim";
      diff = {
        tool = "difftastic";
        colorMoved = "default";
        elisp = { xfuncname = "^\\((((def\\S+)|use-package)\\s+\\S+)"; };
        clojure = { xfuncname = "^\\((def\\S+\\s+\\S+)"; };
      };
      difftool = {
        prompt = false;
        difftastic = { cmd = ''difft "$LOCAL" "$REMOTE"''; };
      };
      pull = {
        ff = "only";
        rebase = false;
      };
      push.autoSetupRemote = true;
      rebase = {
        autostash = true;
      };
    };
    delta = {
      enable = true;
      options = {
        line-numbers = true;
        navigate = true;
        light = false;
        file-style = "bold yellow ul";
        hunk-header-line-number-style = "brightyellow";
      };
    };
    ignores = [
      ".lein-repl-history"
      ".lsp"
      ".rebel_readline_history"
      ".cider-repl-history"
      "nohup.out"
      "*.elc"
      "*.eln"
      "*~"
    ];
    signing = {
      key = "9B436B1477A879C26CDB6604C171251002C200F2";
      signByDefault = true;
    };
  };
}

