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
  };

  gcp-iap-proxy = (pkgs.callPackage ../../scripts/gcp-iap-proxy.nix { inherit pkgs; });
  instance-info = (pkgs.callPackage ../../scripts/instance-info.nix { inherit pkgs; });
in
{
  imports = [
    ./aws.nix
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

  home.shellAliases = {
    update-gcp-ssh = "gcloud compute config-ssh --ssh-config-file=~/.ssh/config_local";
  };

  home.packages = with pkgs; [
    aws-sso-util
    cdktf-cli
    cli-tools
    copilot-language-server
    coffeescript
    instance-info
    gcp-iap-proxy
    git-remote-codecommit
    google-cloud-sdk
    lmdb-cli
    msgpack-tools
    nodejs
    nodePackages.aws-cdk
    opentofu
    pacs
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
        proxyCommand = "${lib.getExe gcp-iap-proxy} %n %p";
        serverAliveInterval = 5;
        sendEnv = [
          "ADZERK_*"
        ];
      };
      "pg1-*" = {
        forwardAgent = true;
        proxyCommand = "${lib.getExe gcp-iap-proxy} %n %p";
        serverAliveInterval = 5;
        sendEnv = [
          "ADZERK_*"
        ];
      };
      "scylladb-*" = {
        forwardAgent = true;
        proxyCommand = "${lib.getExe gcp-iap-proxy} %n %p";
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
      packcat () { ${lib.getExe' pkgs.zstd "zstdcat"} "''${1}" | ${lib.getExe pkgs.gnused} '1d;$d' | ${lib.getExe' pkgs.msgpack-tools "msgpack2json"} -c | ${lib.getExe pkgs.jq} | ${lib.getExe pkgs.bat} }
    '';
  };
}
