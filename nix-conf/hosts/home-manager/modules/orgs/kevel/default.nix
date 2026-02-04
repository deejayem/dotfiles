{
  config,
  lib,
  pkgs,
  ...
}:
let
  envSecrets = {
    ADZERK_GITHUB_PACKAGES_AUTH_TOKEN = "adzerk-packages";
    OPENAI_API_TOKEN = "openai-api";
    AWS_DEFAULT_SSO_START_URL = "sso-start-url";
    GCP_DELIVERY_DEV = "gcp-delivery-dev";
    GCP_DELIVERY_PROD = "gcp-delivery-prod";
  };

  awk = lib.getExe pkgs.gawk;
  bat = lib.getExe pkgs.bat;
  gcp-iap-proxy-bin = lib.getExe pkgs.gcp-iap-proxy;
  gcloud = lib.getExe pkgs.google-cloud-sdk;
  grep = lib.getExe pkgs.gnugrep;
  jq = lib.getExe pkgs.jq;
  msgpack2json = lib.getExe' pkgs.msgpack-tools "msgpack2json";
  sed = lib.getExe pkgs.gnused;
  zstdcat = lib.getExe' pkgs.zstd "zstdcat";
in
{
  imports = [
    ./aws-config.nix
  ];

  programs.home-manager.enable = true;

  home.username = "djm";
  home.homeDirectory = "/Users/djm";

  home.sessionVariables = {
    AWS_DEFAULT_SSO_REGION = "us-east-1";
  };

  home.sessionPath = [
    "$HOME/.npm-global/bin"
    "$HOME/src/kevel/cli-tools/micha"
  ];

  home.packages = with pkgs; [
    aws-cdk-cli
    aws-sso-util
    cdktf-cli
    claude-code
    cli-tools
    codex
    copilot-language-server
    coffeescript
    instance-info
    firebase-tools
    gcp-iap-proxy
    git-remote-codecommit
    google-cloud-sdk
    lmdb-cli
    msgpack-tools
    nodejs
    opentofu
    pacs
    python3
    ruby
    tailscale
    terraform
  ];

  # TODO
  home.file = {
    ".npmrc".text = ''
      @adzerk:registry=https://npm.pkg.github.com/
      //npm.pkg.github.com/:_authToken=''${ADZERK_GITHUB_PACKAGES_AUTH_TOKEN}
      prefix=~/.npm-global
    '';
    ".cdk.json".text = ''
      {
        "context": {
          "cli-telemetry": false
        }
      }
    '';
  };

  sops.templates."nix-netrc" = {
    content = ''
      machine npm.pkg.github.com
          login x-access-token
          password ${config.sops.placeholder."kevel/github/packages-token"}

      machine api.github.com
          login x-access-token
          password ${config.sops.placeholder."kevel/github/api-token"}

      machine github.com
          login x-access-token
          password ${config.sops.placeholder."kevel/github/api-token"}
    '';
    mode = "0400";
    path = "${config.xdg.configHome}/nix/netrc";
  };

  programs.granted = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.java = {
    enable = true;
  };

  programs.git = {
    signing.signByDefault = lib.mkForce false;
    includes = lib.mkForce [
      { path = config.age.secrets."kevel/git/user".path; }
      {
        path = config.age.secrets."git/user".path;
        condition = "gitdir:~/src/ext/";
      }
      {
        path = config.age.secrets."git/user".path;
        condition = "gitdir:~/dotfiles/";
      }
      {
        contents = {
          commit.gpgSign = true;
          tag.gpgSign = true;
        };
        condition = "gitdir:~/src/ext/";
      }
      {
        contents = {
          commit.gpgSign = true;
          tag.gpgSign = true;
        };
        condition = "gitdir:~/dotfiles/";
      }
    ];
    ignores = [
      ".envrc"
      ".clj-kondo"
      "shell.nix"
      ".direnv"
      ".dir-locals.el"
    ];
  };
  programs.ssh = {
    matchBlocks = {
      # aws
      "i-*" = {
        user = "ubuntu";
        proxyCommand = "ssh-ssm.sh %h %r";
        identityFile = "~/.ssh/ssm-ssh-tmp";
        userKnownHostsFile = "/dev/null";
        forwardAgent = true;
        serverAliveInterval = 5;
        sendEnv = [
          "AWS_*"
          "ADZERK_*"
        ];
        extraOptions = {
          "ConnectTimeout" = "30";
          "BatchMode" = "yes";
          "LogLevel" = "QUIET";
          "StrictHostKeyChecking" = "no";
        };
      };

      # gcp
      "gcp1-*" = {
        forwardAgent = true;
        proxyCommand = "${gcp-iap-proxy-bin} %n %p";
        serverAliveInterval = 5;
        sendEnv = [
          "ADZERK_*"
        ];
      };
      "pg1-*" = {
        forwardAgent = true;
        proxyCommand = "${gcp-iap-proxy-bin} %n %p";
        serverAliveInterval = 5;
        sendEnv = [
          "ADZERK_*"
        ];
      };
      "scylladb-*" = {
        forwardAgent = true;
        proxyCommand = "${gcp-iap-proxy-bin} %n %p";
        serverAliveInterval = 5;
        sendEnv = [
          "ADZERK_*"
        ];
      };

      # orbstack
      "*.orb.local" = {
        identityFile = "~/.orbstack/ssh/id_ed25519";
        forwardAgent = true;
        sendEnv = [
          "AWS_*"
          "ADZERK_*"
        ];
      };
    };
  };

  programs.starship.settings.env_var.TICKET = {
    format = "[$env_value]($style) ";
    style = "red bold dimmed";
  };

  programs.zsh = {
    envExtra = lib.concatStringsSep "\n" (
      lib.mapAttrsToList (envName: secretName: ''
        if [ -e ${config.age.secrets."kevel/env/${secretName}".path} ]; then
          export ${envName}=$(<${config.age.secrets."kevel/env/${secretName}".path})
        fi
      '') envSecrets
    );

    initContent = ''
      source "${pkgs.google-cloud-sdk}/share/zsh/site-functions/_gcloud"

      # Prevent spaces from being escaped
      # e.g. gcloud compute ssh foo<tab> becomes gcloud compute ssh foo-bar\ --zone=us-east1-a
      _complete_gcloud() {
        local -x COMP_POINT COMP_CWORD
        local -a COMP_WORDS COMPREPLY
        local -x COMP_LINE="$words"

        (( COMP_POINT = ''${#COMP_LINE} ))
        (( COMP_CWORD = CURRENT - 1 ))
        COMP_WORDS=( $words )

        _python_argcomplete ${gcloud}

        local -a fixed=()
        local m
        for m in "''${COMPREPLY[@]}"; do
            fixed+=("''${m//\\/}")
        done

        compadd -Q -S ' ' -- "''${fixed[@]}"
      }

      compdef _complete_gcloud gcloud

      # ssh completion for gcp hosts
      _gcp_ssh() {
        local prefix project cache_file
        local -a instances config_hosts

        case "$words[CURRENT]" in
            gcp1-*) prefix=gcp1; project="$GCP_DELIVERY_DEV" ;;
            pg1-*)  prefix=pg1;  project="$GCP_DELIVERY_PROD" ;;
            *)      _ssh "$@"; return ;;
        esac

        [[ -z "$project" ]] && { _ssh "$@"; return }

        cache_file="''${XDG_CACHE_HOME:-$HOME/.cache}/gcp-instances-$prefix"

        if [[ ! -f "$cache_file" ]]; then
            ${gcloud} compute instances list --project="$project" --format='value(name)' 2>/dev/null > "$cache_file"
        elif [[ $(( $(date +%s) - $(stat -f%m "$cache_file" 2>/dev/null || stat -c%Y "$cache_file") )) -gt 300 ]]; then
            { ${gcloud} compute instances list --project="$project" --format='value(name)' 2>/dev/null > "$cache_file" } &!
        fi

        instances=(''${(f)"$(<$cache_file)"})

        config_hosts=(''${(f)"$(${grep} -h '^Host ' ~/.ssh/config ~/.ssh/config.local ~/.ssh/config.d/* 2>/dev/null | ${awk} '{print $2}' | ${grep} "^''${prefix}-")"})

        compadd -Q -- "''${instances[@]}" "''${config_hosts[@]}"
      }

      compdef _gcp_ssh ssh
    '';

    siteFunctions = {
      packcat = ''${zstdcat} "$1" | ${sed} '1d;$d' | ${msgpack2json} -c | ${jq} | ${bat}'';
    };
  };
}
