{
  inputs,
  pkgs,
  lib,
  ...
}:
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
        "vscode"
        "zoom"
      ];
    overlays = [
      inputs.self.overlays.additions
      inputs.self.overlays.lazy-flakes
      inputs.self.overlays.modifications
      inputs.self.overlays.unstable-packages
    ];
  };
  nix = {
    package = pkgs.nix;
    settings = {
      experimental-features = "nix-command flakes";
    };
  };
}
