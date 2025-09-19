{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

with lib;
let
  mopidyExtensions = with pkgs; [
    mopidy-iris
    mopidy-local
    mopidy-mpd
    mopidy-muse
    mopidy-ytmusic
  ];

  # https://github.com/nix-community/home-manager/blob/ce563f591195cf363bca382fe02ea5ca87754773/modules/services/mopidy.nix#L22
  mopidy-with-extensions = pkgs.buildEnv {
    name = "mopidy-with-extensions-${pkgs.mopidy.version}";
    paths = closePropagation mopidyExtensions;
    pathsToLink = [ "/${pkgs.mopidyPackages.python.sitePackages}" ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      makeWrapper ${pkgs.mopidy}/bin/mopidy $out/bin/mopidy \
        --prefix PYTHONPATH : $out/${pkgs.mopidyPackages.python.sitePackages}
    '';
  };
in
{
  imports = [ ./dev-common.nix ];

  home.packages = with pkgs; [
    awscli2
    cacert
    caddy
    coreutils
    curl
    diffutils
    ((emacsPackagesFor emacs-macport).emacsWithPackages (ps: [
      ps.vterm
      ps.multi-vterm
    ]))
    findutils
    gh
    gh-dash
    gnused
    iterm2
    #mopidy-with-extensions
    #mpdscribble
    #mpc-cli
    #mpd
    #ncmpcpp
    nix
    pinentry_mac
    pgcli
    pgformatter
    #pms
    poetry
    postgresql
    podman
    procps
    #python310Packages.sqlparse
    redis
    sqls
    ssh-over-ssm
    ssm-session-manager-plugin
    #vimpc
    wget
    _1password-gui

    (pkgs.callPackage ./scripts/darwin-update.nix { inherit pkgs inputs; })
  ];

  home.sessionVariables = {
    NH_DARWIN_FLAKE = "/etc/nix-darwin";
  };

  nix.settings = {
    sandbox = true;
    keep-outputs = true;
    keep-derivations = true;
  };

  programs.bat.extraPackages = with pkgs.bat-extras; [
    (prettybat.override {
      withClangTools = false;
      withRustFmt = false;
    })
  ];

  programs.ghostty = {
    enable = true;
    enableZshIntegration = true;
    package = pkgs.ghostty-bin;
    settings = {
      font-family = "MesloLGS Nerd Font";
      font-size = 13;
      copy-on-select = "clipboard";
      # This is proposed syntax for the future, but ghostty is unusable until it's implemented
      #key-remap = [ "ctrl=super" "super=ctrl" ];
      keybind = [
        "shift+insert=paste_from_clipboard"
      ];
    };
  };

  home.shellAliases = {
    notify_success = ''( osascript -e 'display notification "The command finished" with title "Success"' && afplay /System/Library/Sounds/Ping.aiff && say done  )'';
    notify_failure = ''( osascript -e 'display notification "The command failed" with title "Failure"' && afplay /System/Library/Sounds/Sosumi.aiff && say failed  )'';
    notify = "notify_success || notify_failure";
    ltn = "lein test && notify";
  };

  # TODO is this a good idea?
  #programs.zsh.shellAliases = { emacs = "${emacs-plus-with-packages}/Applications/Emacs.app/Contents/MacOS/Emacs"; };
}
