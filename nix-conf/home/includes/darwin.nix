{ config, pkgs, ... }:
{
  imports = [
    ./dev-common.nix
  ];

  home.packages = with pkgs; [
    awscli2
    coreutils
    curl
    diffutils
    ((emacsPackagesFor emacsMacport).emacsWithPackages(ps: [ ps.vterm ]))
    findutils
    gh
    gh-dash
    #gnused
    #adoptopenjdk-hotspot-bin-8
    #lima
    minikube
    mopidy
    mopidy-iris
    #mopidy-local
    mopidy-mpd
    mopidy-muse
    mopidy-ytmusic
    mpdscribble
    mpc-cli
    mpd
    mpv
    mu
    ncmpcpp
    nix # on darwin we are not using nixos (duh)
    nodejs
    openvpn
    pinentry_mac
    pgcli
    pgformatter
    pms
    postgresql
    #python310Packages.sqlparse
    sqls
    vimpc
    wget
  ];

  nix.settings = {
    sandbox = true;
    keep-outputs = true;
    keep-derivations = true;
  };

  programs.java = {
    enable = true;
    #package = (pkgs.jdk8.overrideAttrs (_: { postPatch = "ln -nsf ../zulu-8.jdk/Contents/Home/man man"; }));
  };
}

