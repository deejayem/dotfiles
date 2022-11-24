{ config, lib, pkgs, ... }:
let
  email = builtins.readFile "${config.home.homeDirectory}/email.txt";
  otmEmail = builtins.readFile "${config.home.homeDirectory}/otm_email.txt";
in
{
  imports = [ 
    ./includes/darwin.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "djm";
  home.homeDirectory = "/Users/djm";

  programs.git = {
    signing.signByDefault = lib.mkForce false;
    userEmail = lib.mkForce otmEmail;
    includes = [
      { contents = { commit.gpgSign = true; user.email = email; }; condition = "gitdir:~/src/personal/"; }
      { contents = { commit.gpgSign = true; user.email = email; }; condition = "gitdir:~/dotfiles/"; }
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
  programs.ssh.matchBlocks = {
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
