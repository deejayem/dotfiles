{ pkgs, lib, ... }:
let
  awk = lib.getExe pkgs.gawk;
  fzf = lib.getExe pkgs.fzf;
in
{
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    shellAliases = {
      # Restrict matches to subdirs of the current one (https://github.com/skywind3000/z.lua/blob/ef9a49d73d2b4f262c6fbb23262253dcda7c19a7/README.md#tips)
      zz = "z $PWD";
    };

    siteFunctions = {
      __zoxide_cd = ''
        setopt localoptions PUSHDSILENT
        \builtin pushd -- "$@"
      '';

      zs = ''
        local idx
        idx=$(dirs -v | ${fzf} --height=40% | ${awk} '{print $1}') || return
        [[ -n "$idx" ]] && __zoxide_cd ~$idx
      '';
    };
  };
}
