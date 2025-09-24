{ pkgs, ... }:

{
  programs.ghostty = {
    enable = true;
    enableZshIntegration = true;
    # On darwin we will install this with nix-darwin instead
    package = if pkgs.stdenv.isDarwin then null else pkgs.ghostty;

    settings = {
      font-family = "MesloLGS Nerd Font";
      font-size = 8;
      copy-on-select = "clipboard";
      # This is proposed syntax for the future, but ghostty is unusable on darwin until it's implemented
      #key-remap = [ "ctrl=super" "super=ctrl" ];
      keybind = [
        "shift+insert=paste_from_clipboard"
      ];

      background = "#000000";
      foreground = "#ffffff";
      cursor-color = "#ffffff";
      selection-background = "#c1ddff";
      selection-foreground = "#000000";

      palette = [
        "0=#2e3436"
        "1=#cc0000"
        "2=#4e9a06"
        "3=#c4a000"
        "4=#3465a4"
        "5=#75507b"
        "6=#06989a"
        "7=#d3d7cf"
        "8=#555753"
        "9=#ef2929"
        "10=#8ae234"
        "11=#fce94f"
        "12=#729fcf"
        "13=#ad7fa8"
        "14=#34e2e2"
        "15=#eeeeec"
      ];
    };
  };
}
