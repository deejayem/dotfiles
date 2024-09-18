{
  config,
  lib,
  pkgs,
  ...
}:
let
  hcr = pkgs.callPackage ./scripts/hm-changes-report.nix { inherit config pkgs; };
  scr = pkgs.callPackage ./scripts/system-changes-report.nix { inherit config pkgs; };
  unstable = import <unstable> { };

  nix-search = (
    pkgs.buildGoModule {
      pname = "nix-search";
      version = "0.3.1";
      src = pkgs.fetchFromGitHub {
        owner = "diamondburned";
        repo = "nix-search";
        rev = "e616ac1c82a616fa6e6d8c94839c5052eb8c808d";
        hash = "sha256-h9yYOjL9i/m0r5NbqMcLMFNnwSKsIgfUr5qk+47pOtc=";
      };
      vendorHash = "sha256-bModWDH5Htl5rZthtk/UTw/PXT+LrgyBjsvE6hgIePY=";
    }
  );
in
{
  imports = [
    ./zsh.nix
    <sops-nix/modules/home-manager/sops.nix>
  ];

  nixpkgs.config.allowUnfreePredicate =
    pkg: builtins.elem (lib.getName pkg) [ "aspell-dict-en-science" ];

  nix = {
    package = pkgs.nix;
    settings = {
      extra-experimental-features = [
        "nix-command"
        "flakes"
      ];
    };
  };

  sops = {
    age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
    defaultSopsFile = builtins.path {
      path = ./../../secrets/home.yaml;
      name = "home-secrets.yaml";
    };
    secrets."ssh_config/oci" = { };
    secrets."git_email_config/default" = { };
  };

  home.sessionVariables = {
    LSP_USE_PLISTS = "true";
  };

  home.packages = with pkgs; [
    hcr
    scr

    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
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
    jd-diff-patch
    jq
    libqalculate
    lscolors
    lynx
    mercurial
    nix-info
    nix-prefetch-git
    nix-prefetch-github
    nix-search
    nixpkgs-review
    nvd
    pass
    rage
    ripgrep
    rlwrap
    sd
    shfmt
    sops
    ssh-to-age
    tealdeer
    tre-command
    tree
    ugrep
    uni
    unzip
    w3m
    yq
    zstd

    unstable.nixfmt-rfc-style
    unstable.wcurl
  ];

  programs.bat = {
    enable = true;
    extraPackages = with pkgs.bat-extras; [
      batdiff
      batgrep
      batman
      batwatch
      batpipe
    ];
    config = {
      style = "full";
      pager = "less -RXF";
      map-syntax = [
        ".ignore:Git Ignore"
        "*.jenkinsfile:Groovy"
      ];
    };
  };

  programs.eza = {
    enable = true;
    git = true;
    icons = true;
    enableBashIntegration = false;
    enableZshIntegration = false;
    enableFishIntegration = false;
    enableIonIntegration = false;
    extraOptions = [
      "--colour=auto"
      "--long"
      "--group-directories-first"
      "--classify"
      "--no-user"
      "--no-time"
      "--no-filesize"
      "--no-permissions"
    ];
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
    plugins = [
      pkgs.vimPlugins.sensible
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
    includes = [
      "~/.ssh/config_local"
      config.sops.secrets."ssh_config/oci".path
    ];
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
      "hb-backup" = {
        hostname = "de1.hashbang.sh";
        identityFile = "~/.ssh/hb_backup_key";
        identitiesOnly = true;
      };
      "bs-backup" = {
        hostname = "ssh.blinkenshell.org";
        port = 2222;
        identityFile = "~/.ssh/bs_backup_key";
        identitiesOnly = true;
      };
      "tt-backup" = {
        hostname = "tilde.team";
        identityFile = "~/.ssh/tt_backup_key";
        identitiesOnly = true;
      };
    };
  };

  programs.git = {
    enable = true;
    userName = "David Morgan";
    includes = [ { path = config.sops.secrets."git_email_config/default".path; } ];
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
      flog = ''!git -c core.pager="diff-so-fancy | less" log''; # usually used with -p
      fshow = ''!git -c core.pager="diff-so-fancy | less" show'';
      fdiff = ''!git -c core.pager="diff-so-fancy | less" diff'';

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
        elisp = {
          xfuncname = "^\\((((def\\S+)|use-package)\\s+\\S+)";
        };
        clojure = {
          xfuncname = "^\\((def\\S+\\s+\\S+)";
        };
      };
      difftool = {
        prompt = false;
        difftastic = {
          cmd = ''difft "$LOCAL" "$REMOTE"'';
        };
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

  programs.lsd = {
    enable = true;
    settings = {
      indicators = true;
      #layout = "oneline";
      sorting.dir-grouping = "first";
      blocks = [
        "git"
        "permission"
        "user"
        "group"
        "size"
        "date"
        "name"
      ];
    };
  };
}
