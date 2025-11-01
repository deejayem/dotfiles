{
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
  imports = [
    ./programs/alacritty.nix
    ./programs/ghostty.nix
    ./programs/karabiner.nix
    ./programs/kitty.nix
  ];

  home.packages = with pkgs; [
    awscli2
    cacert
    caddy
    coreutils
    curl
    diffutils
    findutils
    gh
    gh-dash
    gnused
    mac-app-util
    #mopidy-with-extensions
    #mpdscribble
    #mpc-cli
    #mpd
    #ncmpcpp
    nix
    pgcli
    pgformatter
    #pms
    poetry
    postgresql
    podman
    #python310Packages.sqlparse
    redis
    sqls
    ssh-over-ssm
    ssm-session-manager-plugin
    #vimpc
    watch
    wget

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

  home.shellAliases = {
    notify_success = ''( osascript -e 'display notification "The command finished" with title "Success"' && afplay /System/Library/Sounds/Ping.aiff && say done  )'';
    notify_failure = ''( osascript -e 'display notification "The command failed" with title "Failure"' && afplay /System/Library/Sounds/Sosumi.aiff && say failed  )'';
    notify = "notify_success || notify_failure";
    ltn = "lein test && notify";
  };

  # TODO is this a good idea?
  #programs.zsh.shellAliases = { emacs = "${emacs-plus-with-packages}/Applications/Emacs.app/Contents/MacOS/Emacs"; };
}
