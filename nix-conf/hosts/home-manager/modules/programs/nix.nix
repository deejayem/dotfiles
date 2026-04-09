{
  lib,
  pkgs,
  inputs,
  version,
  ...
}:
let
  jq = lib.getExe pkgs.jq;
  nix = lib.getExe pkgs.nix;
  nixCollectGarbage = lib.getExe' pkgs.nix "nix-collect-garbage";
  nixPrefetchUrl = lib.getExe' pkgs.nix "nix-prefetch-url";
  sed = lib.getExe pkgs.gnused;
  os = if pkgs.stdenv.isDarwin then "darwin" else "nixos";
in
{
  home.packages = with pkgs; [
    comma
    nix-info
    nix-prefetch-git
    nix-prefetch-github
    nix-search
    nixpkgs-hammering
    nixpkgs-reviewFull
    nvd

    unstable.nixfmt

    hm-changes-report
    preview-channel-diff
    system-changes-report
    (pkgs.nix-update.override { inherit inputs os version; })
  ];

  programs.zsh.siteFunctions = {
    denix = ''
      ${sed} -E 's#/nix/store/[^/]+/bin/([[:alnum:]_.+-]+)#\1#g'
    '';

    gcd = ''
      if [ $# -eq 0 ]; then
        echo "Number of days must be specified" >&2
        return 1
      fi
      if ! [[ $1 =~ '^[0-9]+$' ]]; then
        echo "Number of days must be a number" >&2
        return 2
      fi

      local GC_ARGS
      if [ $1 -eq 0 ]; then
        GC_ARGS=(-d)
      else
        GC_ARGS=(--delete-older-than ''${1}d)
      fi

      local DOAS=$(command -v doas || command -v sudo)

      ${nixCollectGarbage} ''${GC_ARGS[@]}
      if [ -n "$DOAS" ]; then
        $DOAS ${nixCollectGarbage} ''${GC_ARGS[@]}
      fi

      df -h
      date
    '';

    nix-prefetch-url-sri = ''${nix} hash convert --hash-algo sha256 --to sri $(${nixPrefetchUrl} --type sha256 "$1")'';

    nixos-eval = "${nix} eval --json $NH_FLAKE#nixosConfigurations.$HOST.config.$1 | ${jq}";

    hm-eval = ''${nix} eval --json $NH_FLAKE#homeConfigurations."$USER@$HOST".config.$1 | ${jq}'';

    darwin-eval = "${nix} eval --json $NH_FLAKE#darwinConfigurations.$HOST.config.$1 | ${jq}";

  };
}
