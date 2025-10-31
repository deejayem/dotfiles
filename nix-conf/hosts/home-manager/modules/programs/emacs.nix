{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) optionals;
  isWorkstation = config.host.role == "workstation";
in
{
  programs.emacs = {
    # On darwin this is installed via nix-darwin
    enable = lib.mkIf pkgs.stdenv.isLinux true;
    package = if isWorkstation then pkgs.emacs-pgtk else pkgs.emacs-nox;
    extraPackages =
      epkgs:
      optionals isWorkstation [
        epkgs.vterm
        epkgs.multi-vterm
      ];
  };
}
