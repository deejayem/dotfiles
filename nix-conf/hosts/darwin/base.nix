{
  pkgs,
  lib,
  system,
  username,
  ...
}:
let
  homeDir = "/Users/${username}";
  # We want pear desktop to remember where it was playing on shutdown,
  # but we want it to start paused. Currently there is no option for
  # this: either it automatically resumes playback on startup, or it
  # forgets what it was playing. So we enabled auto-resume, and the API,
  # then this script automatically pauses it
  pearDesktopPauser = pkgs.writeShellScript "pear-desktop-pauser" ''
    id="pause-on-login"
    api="http://127.0.0.1:26538"
    curl=${lib.getExe pkgs.curl}
    jq=${lib.getExe pkgs.jq}

    token=""
    for _ in {1..60}; do
      token=$("$curl" -fsS -X POST "$api/auth/$id" 2>/dev/null | "$jq" -r .accessToken 2>/dev/null)
      [ -n "$token" ] && [ "$token" != "null" ] && break
      sleep 1
    done
    [ -n "$token" ] && [ "$token" != "null" ] || exit 0

    auth="Authorization: Bearer $token"

    for _ in {1..60}; do
      paused=$("$curl" -fsS -H "$auth" "$api/api/v1/song" 2>/dev/null | "$jq" -r .isPaused 2>/dev/null)
      [ "$paused" = "false" ] && break
      sleep 1
    done

    "$curl" -fsS -o /dev/null -X POST -H "$auth" "$api/api/v1/pause" || true
    "$curl" -fsS -o /dev/null -X POST -H "$auth" -H "Content-Type: application/json" \
      -d '{"seconds":0}' "$api/api/v1/seek-to" || true
  '';
in
{
  nix.settings = {
    netrc-file = homeDir + "/.config/nix/netrc";
    keep-derivations = true;
    keep-outputs = true;
    sandbox = "relaxed";
    trusted-users = [
      username
      "@staff"
    ];
  };

  system.primaryUser = username;
  nixpkgs.hostPlatform = system;
  users.users.${username}.home = homeDir;
  programs.zsh.enable = true;

  security.pam.services.sudo_local = {
    enable = true;
    touchIdAuth = true;
    reattach = true;
  };

  environment.variables = {
    EDITOR = "vim";
  };

  environment.systemPackages = with pkgs; [
    alacritty
    brave
    doll
    ((emacsPackagesFor emacs-macport).emacsWithPackages (ps: [
      ps.vterm
      ps.multi-vterm
    ]))
    firefox
    ghostty-bin
    google-chrome
    iterm2
    maccy
    orbstack
    pear-desktop
    pinentry_mac
    rage
    slack
    vscode
    # zoom-us # TODO: currently causing issues
    _1password-gui
  ];

  fonts.packages = [
    pkgs.aporetic
    pkgs.meslo-lgs-nf
    pkgs.fira-code
  ];

  launchd.user.agents.pear-desktop-pauser = {
    serviceConfig = {
      Label = "com.djm.pear-desktop-pauser";
      ProgramArguments = [ "${pearDesktopPauser}" ];
      RunAtLoad = true;
    };
  };

  services.tailscale = {
    enable = true;
  };

  system.defaults.CustomUserPreferences = {
    "com.tinyspeck.slackmacgap" = {
      AutoUpdate = false;
    };
    "com.xiaogd.Doll" = {
      KeyboardShortcuts_toggleConfigWindow = "{\"carbonKeyCode\":2,\"carbonModifiers\":768}";
      SETTINGS_Show_As_Red_Badge = true;
      SETTING_MONITORED_APP_IDS = "com.tinyspeck.slackmacgap";
    };
    "org.alacritty" = {
      NSUserKeyEquivalents = {
        "Hide alacritty" = "\\0";
      };
    };
  };

  system.activationScripts.postActivation.text = ''
    echo "Restarting Doll"
    pkill Doll || true
    open -a "/Applications/Nix Apps/Doll.app"
  '';

  system.defaults.dock.autohide = true;
  system.defaults.dock.persistent-apps = [
    { app = "/System/Applications/System Settings.app"; }
    { app = "/Applications/Nix Apps/YouTube Music.app"; }
    { app = "/Applications/Nix Apps/Google Chrome.app"; }
    { app = "/Applications/Nix Apps/Firefox.app"; }
    { app = "/Applications/Nix Apps/Brave Browser.app"; }
    { app = "/Applications/Nix Apps/Slack.app"; }
    { app = "/Applications/Nix Apps/Alacritty.app"; }
    { app = "/Applications/Nix Apps/Emacs.app"; }
    # { app = "/Applications/Nix Apps/zoom.us.app"; } # TODO: currently causing issues
    { app = "/Applications/zoom.us.app"; }
    { app = "/Applications/Nix Apps/1Password.app"; }
    { app = "/Applications/Nix Apps/OrbStack.app"; }
  ];

  homebrew = {
    enable = true;
    greedyCasks = true;
    casks = [
      "ankerwork"
      "karabiner-elements" # services.karabiner-elements.enable = true; causes problems with SentinelOne
      "zoom" # pkgs.zoom-us causes problems with SentinelOne
    ];
  };
}
