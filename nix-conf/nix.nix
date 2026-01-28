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
        "claude-code"
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
    };
  };
}
