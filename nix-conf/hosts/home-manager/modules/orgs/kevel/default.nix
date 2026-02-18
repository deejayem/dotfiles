{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  bat = lib.getExe pkgs.bat;
  jq = lib.getExe pkgs.jq;
  msgpack2json = lib.getExe' pkgs.msgpack-tools "msgpack2json";
  sed = lib.getExe pkgs.gnused;
  zstdcat = lib.getExe' pkgs.zstd "zstdcat";
in
{
  imports = [
    ./aws-config.nix
    ./gcp.nix
  ];

  home.sessionVariables = {
    AWS_DEFAULT_SSO_REGION = "us-east-1";
  };

  home.sessionPath = [
    "$HOME/.npm-global/bin"
  ];

  home.packages = with pkgs; [
    aws-cdk-cli
    cdktn-cli
    cli-tools
    coffeescript
    aws-instance-info
    git-remote-codecommit
    lmdb-cli
    msgpack-tools
    nodejs
    opentofu
    pacs
    python3
    ruby
    tailscale
    terraform
    sqlcmd
  ]
  ++ (with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
    claude-code
    codex
    copilot-language-server
  ]);

  # TODO
  home.file = {
    ".npmrc".text = ''
      @adzerk:registry=https://npm.pkg.github.com/
      //npm.pkg.github.com/:_authToken=''${GITHUB_PACKAGES_TOKEN}
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
    dirHashes = {
      ext = "${config.home.homeDirectory}/src/ext";
      kevel = "${config.home.homeDirectory}/src/kevel";
      nixp = lib.mkForce "${config.home.homeDirectory}/src/ext/nixpkgs";
    };
    siteFunctions = {
      packcat = ''${zstdcat} "$1" | ${sed} '1d;$d' | ${msgpack2json} -c | ${jq} | ${bat}'';
    };
  };
}
