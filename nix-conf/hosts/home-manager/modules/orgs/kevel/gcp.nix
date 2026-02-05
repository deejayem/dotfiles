{ lib, pkgs, ... }:
let
  awk = lib.getExe pkgs.gawk;
  gcp-iap-proxy-bin = lib.getExe pkgs.gcp-iap-proxy;
  gcloud = lib.getExe pkgs.google-cloud-sdk;
  grep = lib.getExe pkgs.gnugrep;
in
{
  home.packages = with pkgs; [
    firebase-tools
    gcp-iap-proxy
    google-cloud-sdk
  ];

  programs.ssh = {
    matchBlocks = {
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
    };
  };

  programs.zsh = {
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
      gcs = ''
        ${gcloud} config set project "$1" && ${gcloud} auth application-default set-quota-project "$1"
      '';
    };
  };
}
