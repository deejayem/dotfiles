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

  home.activation.applicationAliases = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -euo pipefail

    SRC="$HOME/.nix-profile/Applications"
    DST="/Applications"

    changed=0

    find_alias() {
      /usr/bin/osascript - -- "''$1" <<'OSA' || true
  on run argv
    set p to POSIX file (item 1 of argv)
    try
      set a to (p as alias)
      return POSIX path of a
    on error
      return ""
    end try
  end run
  OSA
    }

    # Create aliases for everything in ~/.nix-profile/Applications in /Applications,
    # skipping anything that already exists (as a non-alias), and only replacing
    # things that have changed
    ${pkgs.findutils}/bin/find -H "$SRC" -maxdepth 1 -name '*.app' -print0 |
    while IFS= read -r -d $'\0' app; do
      base="''${app##*/}"
      target="$DST/$base"

      if [ -e "$target" ]; then
        # Check if this is an alias or a real app
        if /usr/bin/mdls -name kMDItemKind "$target" 2>/dev/null | ${pkgs.gnugrep}/bin/grep -q 'Alias$'; then
          current="$(find_alias "$target" || true)"
          if [ -z "''${current:-}" ] || [ "$current" != "$app" ]; then
            ${pkgs.coreutils}/bin/rm -rf "$target"
            ${pkgs.mkalias}/bin/mkalias "$app" "$target"
            changed=1
          fi
        else
          echo "Warning: skipping $target because it exists and is not a finder alias"
          continue
        fi
      else
        ${pkgs.mkalias}/bin/mkalias "$app" "$target"
        changed=1
      fi
    done

    # Only poke LaunchServices/Spotlight if something changed
    if [ "$changed" -eq 1 ]; then
      /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f "$DST" >/dev/null 2>&1 || true
      /usr/bin/mdimport "$DST" >/dev/null 2>&1 || true
    fi
  '';

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
