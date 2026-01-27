{
  config,
  lib,
  pkgs,
  system,
  inputs,
  role,
  version,
  ...
}:
let
  os = lib.last (lib.splitString "-" system);
in
{
  imports = [
    ../../options.nix
    ./home-secrets
    ./programs/bat.nix
    ./programs/emacs.nix
    ./programs/eza.nix
    ./programs/git.nix
    ./programs/lsd.nix
    ./programs/nix-index.nix
    ./programs/nh.nix
    ./programs/nvim.nix
    ./programs/ssh.nix
    ./programs/starship.nix
    ./programs/tmux.nix
    ./programs/zsh.nix
    ./roles/${role}.nix
    ./os/${os}.nix
  ];

  host.role = role;

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
    mu
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

    unstable.nixfmt

    hm-changes-report
    system-changes-report
    (pkgs.nixos-update.override { inherit inputs version; })
  ];

  programs.gpg.enable = true;

  # Temporary workaround
  launchd.agents.sops-nix = pkgs.lib.mkIf pkgs.stdenv.isDarwin {
    enable = true;
    config = {
      EnvironmentVariables = {
        PATH = pkgs.lib.mkForce "/usr/bin:/bin:/usr/sbin:/sbin";
      };
    };
  };
}
