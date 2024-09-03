{ config, lib, pkgs, ... }:

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
in {
  imports = [ ./dev-common.nix ];

  home.packages = with pkgs; [
    awscli2
    cacert
    coreutils
    curl
    diffutils
    ((emacsPackagesFor emacs29-macport).emacsWithPackages
      (ps: [ ps.vterm ps.multi-vterm ]))
    findutils
    gh
    gh-dash
    #gnused
    #mopidy-with-extensions
    #mpdscribble
    #mpc-cli
    #mpd
    #ncmpcpp
    nix # on darwin we are not using nixos (duh)
    nodejs
    pam-reattach
    pinentry_mac
    pgcli
    pgformatter
    #pms
    postgresql
    podman
    #python310Packages.sqlparse
    sqls
    #vimpc
    wget
  ];

  nix.settings = {
    sandbox = true;
    keep-outputs = true;
    keep-derivations = true;
  };
}

