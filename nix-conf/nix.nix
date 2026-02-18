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
      extra-substituters = [
        "https://nix-community.cachix.org"
        "https://cache.numtide.com"
      ];
      extra-trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
      ];
    };
  };
}
