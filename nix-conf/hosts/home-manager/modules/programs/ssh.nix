{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) optionalAttrs;
in
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    includes = [
      "~/.ssh/config_local"
      config.age.secrets."ssh/oci".path
    ];
    settings = {
      "*" = {
        AddKeysToAgent = "yes";
        ForwardAgent = "yes";
        User = "djm";
      }
      // optionalAttrs pkgs.stdenv.isDarwin {
        UseKeychain = "yes";
      };
      "djm.ovh" = {
        HostName = "v.djm.ovh";
        Port = 2222;
      };
      "devio" = {
        HostName = "devio.us";
        User = "deejayem";
        Port = 2222;
      };
      "sdf" = {
        HostName = "sdf.org";
        User = "deejayem";
      };
      "sdfeu" = {
        HostName = "sdf-eu.org";
        User = "deejayem";
      };
      "grex" = {
        HostName = "grex.org";
        User = "deejayem";
      };
      "blinkenshell" = {
        HostName = "ssh.blinkenshell.org";
        Port = 2222;
      };
      "hashbang" = {
        HostName = "de1.hashbang.sh";
      };
      "tilde.institute" = {
        HostName = "tilde.institute";
      };
      "tilde.team" = {
        HostName = "tilde.team";
      };
      "ctrl-c.club" = {
        HostName = "ctrl-c.club";
      };
      "github.com" = {
        HostName = "github.com";
        User = "git";
        IdentityFile = "~/.ssh/id_ed25519";
        IdentitiesOnly = "yes";
      };
      "hb-backup" = {
        HostName = "de1.hashbang.sh";
        IdentityFile = "~/.ssh/hb_backup_key";
        IdentitiesOnly = "yes";
      };
      "bs-backup" = {
        HostName = "ssh.blinkenshell.org";
        Port = 2222;
        IdentityFile = "~/.ssh/bs_backup_key";
        IdentitiesOnly = "yes";
      };
      "tt-backup" = {
        HostName = "tilde.team";
        IdentityFile = "~/.ssh/tt_backup_key";
        IdentitiesOnly = "yes";
      };
    };
  };
}
