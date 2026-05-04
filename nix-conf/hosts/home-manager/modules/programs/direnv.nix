{
  config,
  lib,
  pkgs,
  ...
}:
let
  git = lib.getExe pkgs.git;
in
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    config = {
      whitelist = {
        exact = [ "${config.home.homeDirectory}/src" ];
      };
    };
  };

  home.file."src/.envrc".text = ''
    use_repo_flake() {
      local workdir root repo flake

      workdir="''${DIRENV_WORKDIR:-$PWD}"
      root="$(${git} -C "$workdir" rev-parse --show-toplevel 2>/dev/null || printf '%s\n' "$workdir")"
      repo="$(basename "$root")"
      flake="${config.home.homeDirectory}/dotfiles/nix-conf/repos/$repo"

      [[ -e "$flake/flake.nix" ]] || return 0

      use flake "$flake"
    }

    use_repo_flake
  '';
}
