{ pkgs, ... }:
{
  nix.settings.trusted-users = [
    "djm"
    "@staff"
  ];
  system.stateVersion = 6;
  system.primaryUser = "djm";
  nixpkgs.hostPlatform = "aarch64-darwin";
  users.users.djm.home = "/Users/djm";

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
    doll
    ((emacsPackagesFor emacs-macport).emacsWithPackages (ps: [
      ps.vterm
      ps.multi-vterm
    ]))
    firefox
    ghostty-bin
    google-chrome # chromium is not available on darwin (in nixpkgs)
    iterm2
    orbstack
    pinentry_mac
    slack
    vscode
    youtube-music
    zoom-us
    _1password-gui
  ];

  fonts.packages = [
    pkgs.aporetic
    pkgs.meslo-lgs-nf
    pkgs.fira-code
  ];

  system.defaults.CustomUserPreferences = {
    "com.xiaogd.Doll" = {
      KeyboardShortcuts_toggleConfigWindow = "{\"carbonKeyCode\":2,\"carbonModifiers\":768}";
      SETTINGS_Show_As_Red_Badge = true;
      SETTING_MONITORED_APP_IDS = "com.tinyspeck.slackmacgap";
    };
  };

  system.defaults.dock.autohide = true;
  system.defaults.dock.persistent-apps = [
    { app = "/System/Applications/System Settings.app"; }
    { app = "/Applications/Nix Apps/YouTube Music.app"; }
    { app = "/Applications/Nix Apps/Google Chrome.app"; }
    { app = "/Applications/Nix Apps/Firefox.app"; }
    { app = "/Applications/Nix Apps/Slack.app"; }
    { app = "/Applications/Nix Apps/Alacritty.app"; }
    { app = "/Applications/Nix Apps/Emacs.app"; }
    { app = "/Applications/Nix Apps/zoom.us.app"; }
    { app = "/Applications/Nix Apps/1Password.app"; }
    { app = "/Applications/Nix Apps/OrbStack.app"; }
  ];

  homebrew = {
    enable = true;
    greedyCasks = true;
    casks = [
      "ankerwork"
      "aws-vpn-client"
      "karabiner-elements" # services.karabiner-elements.enable = true; causes problems
    ];
  };

}
