{
  config,
  lib,
  pkgs,
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

  # Use the patches from emacs-plus
  emacs-plus = (pkgs.emacs30-pgtk.overrideAttrs (old: {
        patches =
          (old.patches or [])
          ++ [
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/fix-window-role.patch";
              sha256 = "0c41rgpi19vr9ai740g09lka3nkjk48ppqyqdnncjrkfgvm2710z";
            })
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/round-undecorated-frame.patch";
              sha256 = "uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
            })
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
              sha256 = "3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
            })
          ];
      }));

  emacs-plus-with-packages = (pkgs.emacsPackagesFor emacs-plus).emacsWithPackages (ps: [
    ps.vterm
    ps.multi-vterm
  ]);

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
    emacs-plus-with-packages
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
    poetry
    postgresql
    podman
    #python310Packages.sqlparse
    redis
    sqls
    #vimpc
    wget
  ];

  #nixpkgs.config.permittedInsecurePackages = [
  #  "emacs-mac-macport-29.1"
  #  "emacs-mac-macport-with-packages-29.1"
  #];

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

  # TODO is this a good idea?
  #programs.zsh.shellAliases = { emacs = "${emacs-plus-with-packages}/Applications/Emacs.app/Contents/MacOS/Emacs"; };
}
