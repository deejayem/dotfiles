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
    } // lib.optionalAttrs pkgs.stdenv.isDarwin {
      aws-open = ''
        local profile="''${1:-$AWS_PROFILE}"
        [[ -n "$profile" ]] || { print -u2 "aws-open: no profile specified and AWS_PROFILE not set"; return 2; }

        local start_url account_id role_name
        start_url=$(${aws} configure get sso_start_url --profile "$profile") || { print -u2 "aws-open: cannot read sso_start_url for $profile"; return 1; }
        account_id=$(${aws} configure get sso_account_id --profile "$profile") || { print -u2 "aws-open: cannot read sso_account_id for $profile"; return 1; }
        role_name=$(${aws} configure get sso_role_name --profile "$profile") || { print -u2 "aws-open: cannot read sso_role_name for $profile"; return 1; }

        open "''${start_url}/#/console?account_id=''${account_id}&role_name=''${role_name}"
      '';
    };

    initContent = lib.mkIf pkgs.stdenv.isDarwin ''
      _aws-open() {
        local -a profiles
        profiles=( ''${(f)"$(${aws} configure list-profiles 2>/dev/null)"} )
        compadd -a profiles
      }
      compdef _aws-open aws-open
    '';
  };
}
