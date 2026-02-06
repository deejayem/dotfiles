{ config, pkgs, ... }:
{
  programs.ripgrep.enable = true;

  programs.ripgrep-all.enable = config.host.role == "workstation";
}

