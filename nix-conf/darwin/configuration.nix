{ inputs, pkgs, ... }:
{
  nix.settings.trusted-users = [
    "djm"
    "@staff"
  ];
  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;
  system.stateVersion = 6;
  #system.primaryUser = "djm"; # required to update com.apple.symbolichotkeys
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
  fonts.packages = [
    pkgs.aporetic
    pkgs.meslo-lgs-nf
    pkgs.fira-code
  ];
}
