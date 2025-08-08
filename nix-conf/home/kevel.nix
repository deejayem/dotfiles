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

  # TODO move to darwin.nix
  home.shellAliases = {
    notify_success = ''( osascript -e 'display notification "The command finished" with title "Success"' && afplay /System/Library/Sounds/Ping.aiff && say done  )'';
    notify_failure = ''( osascript -e 'display notification "The command failed" with title "Failure"' && afplay /System/Library/Sounds/Sosumi.aiff && say failed  )'';
    notify = "notify_success || notify_failure";
    ltn = "lein test && notify";
  };

  home.packages = with pkgs; [
    granted
    nodejs
  ];

  # TODO
  #home.file = {
  #};

  sops.secrets = {
    "git_email_config/kevel" = { };
    "ssh_config/kevel" = { };
  };

  programs.java = {
    enable = true;
  };

  # TODO
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
  # TODO don't need different accounts, just different emails
  programs.ssh = {
    includes = [ config.sops.secrets."ssh_config/kevel".path ];
  };

  home.stateVersion = "25.05";
}
