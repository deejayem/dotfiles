{
  inputs,
  pkgs,
  lib,
  ...
}:
let
  nix-plugins = import ./plugins/nix-plugins.nix { inherit pkgs; };
in
{
  nixpkgs = {
    config.allowUnfreePredicate =
      pkg:
      builtins.elem (lib.getName pkg) [
        "1password"
        "aspell-dict-en-science"
        "copilot-language-server"
        "corefonts"
        "google-chrome" # only for darwin where chromium is not available
        "hplip"
        "orbstack"
        "slack"
        "terraform"
        "vscode"
        "zoom"
      ];
    overlays = builtins.attrValues inputs.self.overlays;
  };

  nix = {
    package = pkgs.nix;
    settings = {
      experimental-features = "nix-command flakes";
      plugin-files = "${nix-plugins}/lib/nix/plugins";
      extra-builtins-file = [ ./plugins/extra-builtins.nix ];
    };
  };
}
