{ pkgs, ... }:
{
  nix.settings.trusted-users = [
    "djm"
    "@staff"
  ];
  system.stateVersion = 6;
  system.primaryUser = "djm";
  system.keyboard.enableKeyMapping = true;
  system.keyboard.userKeyMapping = [
    {
      HIDKeyboardModifierMappingSrc = 30064771296;
      HIDKeyboardModifierMappingDst = 30064771299;
    }
    {
      HIDKeyboardModifierMappingSrc = 30064771299;
      HIDKeyboardModifierMappingDst = 30064771296;
    }
  ];
  #system.defaults.CustomUserPreferences = {
  #  "com.apple.symbolichotkeys" = {
  #    AppleSymbolicHotKeys = {
  #      "60" = {
  #        enabled = 0;
  #      };
  #      "61" = {
  #        enabled = 0;
  #      };
  #    };
  #  };
  #};
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
    ((emacsPackagesFor emacs-macport).emacsWithPackages (ps: [
      ps.vterm
      ps.multi-vterm
    ]))
    firefox
    google-chrome # chromium is not available on darwin (in nixpkgs)
    iterm2
    pinentry_mac
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

  system.defaults.dock.persistent-apps = [
    { app = "/System/Applications/System Settings.app"; }
    { app = "/Applications/Nix Apps/Google Chrome.app"; }
    { app = "/Applications/Nix Apps/Firefox.app"; }
    { app = "/Applications/Nix Apps/Slack.app"; }
    { app = "/Applications/Nix Apps/iTerm2.app"; }
    { app = "/Applications/Nix Apps/Emacs.app"; }
    { app = "/Applications/Nix Apps/zoom.us.app"; }
    { app = "/Applications/Nix Apps/1Password.app"; }
    { app = "/Applications/Nix Apps/OrbStack.app"; }
  ];

  homebrew = {
    enable = true;
    greedyCasks = true;
    brews = [
      "cdktf" # Currently broken in nixpkgs
      "msgpack-tools" # Currently broken in nixpkgs
    ];
    casks = [
      "ankerwork"
      "aws-vpn-client"
      "gcloud-cli"
    ];
  };
}
