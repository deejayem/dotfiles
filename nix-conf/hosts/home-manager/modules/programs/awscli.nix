{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  aws = lib.getExe pkgs.awscli2;
  jq = lib.getExe pkgs.jq;

in
{
  home.packages = with pkgs; [
    ssh-over-ssm
    ssm-session-manager-plugin
  ];

  programs.awscli.enable = true;

  programs.zsh = {
    plugins = with pkgs; [
      {
        name = "omz-aws";
        src = fetchFromGitHub {
          owner = "ohmyzsh";
          repo = "ohmyzsh";
          rev = "2423b7a12dc4624a2d8a7c58be4ac75cb82fd8c7";
          sha256 = "fCAwg6fzXw/mEa+xEnSCK88/ba8nR0FNY2tQ62CchbQ=";
        };
        file = "plugins/aws/aws.plugin.zsh";
      }
    ];

    siteFunctions = {
      awsget = ''${aws} s3 cp "$1" "''${2:-.}"'';

      aws_logged_in = ''
        local cache_dir="$HOME/.aws/sso/cache"
        [[ -d "$cache_dir" ]] || return 1

        ${jq} -e '
          select(.startUrl?)
          | .expiresAt
          | sub("UTC$"; "Z")
          | strptime("%Y-%m-%dT%H:%M:%SZ")
          | mktime > now
        ' "$cache_dir"/*.json(N) >/dev/null 2>&1
      '';

      aspl = ''
        [[ -n "$1" ]] || { print -u2 "aspl: missing profile"; return 2; }
        local profile="$1"
        shift || true

        if ! aws_logged_in; then
          echo "Logging in to profile $profile"
          asp "$profile" login
        else
          echo "Setting profile $profile"
          asp "$profile" "$@"
        fi
      '';
    };
  };
}
