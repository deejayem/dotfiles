{ pkgs, username, ... }:
{
  nix.settings.trusted-users = [
    username
    "@staff"
  ];

  system.primaryUser = username;
  nixpkgs.hostPlatform = "aarch64-darwin";
  users.users.${username}.home = "/Users/${username}";
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
    pear-desktop
    pinentry_mac
    rage
    slack
    vscode
    zoom-us
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
