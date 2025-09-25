{ lib, pkgs, ... }:
{
  programs.alacritty = {
    enable = true;
    # On darwin we will install this with nix-darwin instead
    package = if pkgs.stdenv.isDarwin then null else pkgs.alacritty;
    settings = {
      font = {
        normal.family = "MesloLGS NF";
        size = 12;
      };
      
      selection = {
        save_to_clipboard = true;
      };
      
      keyboard.bindings = [
        {
          key = "Insert";
          mods = "Shift";
          action = "Paste";
        }
      ];
      
      window = lib.optionalAttrs pkgs.stdenv.isDarwin {
        option_as_alt = "OnlyLeft";
      };
      
      colors = {
        primary = {
          background = "#000000";
          foreground = "#c7c7c7";
        };
        
        cursor = {
          text = "#000000";
          cursor = "#c7c7c7";
        };
        
        selection = {
          text = "#000000";
          background = "#b5d5ff";
        };
        
        normal = {
          black = "#000000";
          red = "#c91b00";
          green = "#00c200";
          yellow = "#c7c400";
          blue = "#0225c7";
          magenta = "#ca30c7";
          cyan = "#00c5c7";
          white = "#c7c7c7";
        };
        
        bright = {
          black = "#686868";
          red = "#ff6e67";
          green = "#5ff967";
          yellow = "#fefb67";
          blue = "#6871ff";
          magenta = "#ff76ff";
          cyan = "#5ffdff";
          white = "#feffff";
        };
      };
    };
  };
}
