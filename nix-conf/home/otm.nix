{ config, lib, pkgs, ... }:
{
  imports = [ 
    ./includes/darwin.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "dmorgan";
  home.homeDirectory = "/Users/dmorgan";

  sops.secrets = {
    "git_email_config/otm" = { };
    "ssh_config/otm" = { };
  };

  programs.git = {
    signing.signByDefault = lib.mkForce false;
    includes = lib.mkForce [
      { path = config.sops.secrets."git_email_config/otm".path; }
      { path = config.sops.secrets."git_email_config/default".path; condition = "gitdir:~/src/personal/"; }
      { path = config.sops.secrets."git_email_config/default".path; condition = "gitdir:~/dotfiles/"; }
      { contents = { commit.gpgSign = true; tag.gpgSign = true; }; condition = "gitdir:~/src/personal/"; }
      { contents = { commit.gpgSign = true; tag.gpgSign = true; }; condition = "gitdir:~/dotfiles/"; }
    ];
    extraConfig = {
      github.user = "david-morgan-otm";
    };
    ignores = [
      ".envrc"
      ".clj-kondo"
      "shell.nix"
      ".direnv"
      ".dir-locals.el"
      "browser-tests/package-lock.json"
      "resources/react-app/package-lock.json"
      "resources/next/package-lock.json"
    ];
  };
  programs.ssh = {
    includes = [ config.sops.secrets."ssh_config/otm".path ];
    matchBlocks = {
      "github.com" = lib.mkForce {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_rsa";
        identitiesOnly = true;
      };
      "github.com-personal" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_ed25519";
        identitiesOnly = true;
      };
    };
  };
  
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
}
