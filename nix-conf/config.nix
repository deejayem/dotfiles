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
        "zoom"
      ];
    overlays = [ outputs.overlays.unstable-packages ];
  };
  nix = {
    package = pkgs.nix;
    settings = {
      experimental-features = "nix-command flakes";
    };
  };
}
