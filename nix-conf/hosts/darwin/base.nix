{
  pkgs,
  system,
  username,
  ...
}:
let
  homeDir = "/Users/${username}";
in
{
  nix.settings = {
    netrc-file = homeDir + "/.config/nix/netrc";
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
