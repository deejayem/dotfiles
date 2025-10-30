{
  config,
  pkgs,
  inputs,
  version,
  ...
}:
{
  imports = [
    ./git.nix
    ./nvim.nix
    ./ssh.nix
    ./starship.nix
    ./zsh.nix
    inputs.sops-nix.homeManagerModules.sops
  ];

  sops = {
    age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
    defaultSopsFile = builtins.path {
      path = ./secrets.yaml;
      name = "home-secrets.yaml";
    };
  };

  home.sessionPath = [
    "$HOME/bin"
    "$HOME/.local/bin"
    "$HOME/go/bin"
  ];

  home.sessionVariables = {
    LSP_USE_PLISTS = "true";
  };

  home.packages = with pkgs; [
    (aspellWithDicts (
      dicts: with dicts; [
        en
        en-computers
        en-science
      ]
    ))
    bandwhich
    bottom
    broot
    comma
    curl
    diff-so-fancy
    difftastic
    duf
    dust
    ea
    elinks
    entr
    fd
    file
    fzf
    gnupg
    gopass
    htop
    inetutils
    ispell
    isync
    jd-diff-patch
    jq
    json-table
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
    procs
    pstree
    pv
    rage
    ripgrep
    rlwrap
    sd
    sops
    shfmt
    shellcheck
    ssh-to-age
    tlrc
    tre-command
    tree
    ugrep
    uni
    unzip
    w3m
    xan
    yq
    zip
    zstd

    unstable.nixfmt-rfc-style

    (pkgs.callPackage ./scripts/hm-changes-report.nix { inherit config pkgs; })
    (pkgs.callPackage ./scripts/system-changes-report.nix { inherit config pkgs; })
    (pkgs.callPackage ./scripts/nixos-update.nix { inherit pkgs inputs version; })
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
    icons = "auto";
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
  programs.nix-index.enable = true;

  programs.lsd = {
    enable = true;
    enableZshIntegration = false; # don't set aliases
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

  programs.nh = {
    enable = true;
    flake = "${config.home.homeDirectory}/dotfiles/nix-conf";
  };
}
