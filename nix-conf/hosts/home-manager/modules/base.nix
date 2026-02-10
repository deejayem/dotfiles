{
  config,
  lib,
  pkgs,
  system,
  role,
  org,
  ...
}:
let
  os = lib.last (lib.splitString "-" system);
in
{
  imports = [
    ../../options.nix
    ./home-secrets
    ./programs/atuin.nix
    ./programs/bat.nix
    ./programs/ea.nix
    ./programs/emacs.nix
    ./programs/eza.nix
    ./programs/fzf.nix
    ./programs/git.nix
    ./programs/lsd.nix
    ./programs/nix.nix
    ./programs/nix-index.nix
    ./programs/nh.nix
    ./programs/nvim.nix
    ./programs/ripgrep.nix
    ./programs/ssh.nix
    ./programs/starship.nix
    ./programs/tmux.nix
    ./programs/zoxide.nix
    ./programs/zsh.nix
    ./roles/${role}.nix
    ./os/${os}.nix
  ]
  ++ lib.optionals (org != null) [
    ./orgs/${org}
  ];

  host.role = role;
  host.org = org;

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
    curl
    duf
    dust
    elinks
    entr
    file
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
    pass
    procs
    pstree
    pv
    rage
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

  ];

  programs.fd.enable = true;

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
