{
  outputs,
  pkgs,
  lib,
  ...
}:
{
  nixpkgs = {
    config.allowUnfreePredicate =
      pkg:
      builtins.elem (lib.getName pkg) [
        "aspell-dict-en-science"
        "corefonts"
        "hplip"
        "vscode"
        "zoom"
      ];
    overlays = [
      outputs.overlays.additions
      outputs.overlays.unstable-packages
    ];
  };
  nix = {
    package = pkgs.nix;
    settings = {
      experimental-features = "nix-command flakes";
    };
  };
}
