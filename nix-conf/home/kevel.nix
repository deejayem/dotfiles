{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
{
  imports = [ ./includes/darwin.nix ];

  programs.home-manager.enable = true;

  home.username = "djm";
  home.homeDirectory = "/Users/djm";

  home.sessionVariables = {
    AWS_DEFAULT_SSO_REGION = "us-east-1";
    AWS_DEFAULT_SSO_START_URL = "https://kevel.awsapps.com/start";
    TERRAFORM_BINARY_NAME = "tofu";
  };

  home.sessionPath = [
    "$HOME/.npm-global/bin"
  ];

  home.packages = with pkgs; [
    aws-sso-util
    coffeescript
    nodejs
    opentofu
  ];

  # TODO
  home.file = {
    ".npmrc".text = ''
      @adzerk:registry=https://npm.pkg.github.com/
      //npm.pkg.github.com/:_authToken=''${ADZERK_GITHUB_PACKAGES_AUTH_TOKEN}
      prefix=~/.npm-global
    '';
  };

  sops.secrets = {
    "git_email_config/kevel" = { };
    "ssh_config/kevel" = { };
    "env/adzerk-packages-token" = { };
  };

  programs.java = {
    enable = true;
  };

  programs.git = {
    signing.signByDefault = lib.mkForce false;
    includes = lib.mkForce [
      { path = config.sops.secrets."git_email_config/kevel".path; }
      {
        path = config.sops.secrets."git_email_config/default".path;
        condition = "gitdir:~/src/ext/";
      }
      {
        path = config.sops.secrets."git_email_config/default".path;
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
      "default.nix"
      ".direnv"
      ".dir-locals.el"
    ];
  };
  programs.ssh = {
    includes = [ config.sops.secrets."ssh_config/kevel".path ];
  };

  programs.granted = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh.envExtra = ''
    export ADZERK_GITHUB_PACKAGES_AUTH_TOKEN=$(<${config.sops.secrets."env/adzerk-packages-token".path})
  '';

  home.stateVersion = "25.05";
}
