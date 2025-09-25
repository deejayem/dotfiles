{ lib, pkgs, ... }:
let
  inherit (lib) optionalAttrs;
in
{
  programs.ghostty = {
    enable = true;
    enableZshIntegration = true;
    # On darwin we will install this with nix-darwin instead
    package = if pkgs.stdenv.isDarwin then null else pkgs.ghostty;

    settings = {
      font-family = "MesloLGS Nerd Font";
      font-size = if pkgs.stdenv.isDarwin then 12 else 8;
      copy-on-select = "clipboard";
      # This is proposed syntax for the future, but for now we achieve this with karabiner
      #key-remap = [ "ctrl=super" "super=ctrl" ];
      keybind = [
        "shift+insert=paste_from_clipboard"
      ];

      background = "#000000";
      foreground = "#c7c7c7";
      cursor-color = "#c7c7c7";
      selection-background = "#b5d5ff";
      selection-foreground = "#000000";
      palette = [
        "0=#000000" # Black
        "1=#c91b00" # Red
        "2=#00c200" # Green
        "3=#c7c400" # Yellow
        "4=#3366cc" # Blue
        "5=#ca30c7" # Magenta
        "6=#00c5c7" # Cyan
        "7=#c7c7c7" # White
        "8=#686868" # Bright Black
        "9=#ff6e67" # Bright Red
        "10=#5ff967" # Bright Green
        "11=#fefb67" # Bright Yellow
        "12=#6871ff" # Bright Blue
        "13=#ff76ff" # Bright Magenta
        "14=#5ffdff" # Bright Cyan
        "15=#feffff" # Bright White
      ];
    }
    // optionalAttrs pkgs.stdenv.isDarwin {
      macos-option-as-alt = "left";
    };
  };
}
