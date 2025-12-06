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
  sops.secrets."ssh_config/oci" = { };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    includes = [
      "~/.ssh/config_local"
      config.sops.secrets."ssh_config/oci".path
    ];
    matchBlocks = {
      "*" = {
        addKeysToAgent = "yes";
        forwardAgent = true;
        user = "djm";
      }
      // optionalAttrs pkgs.stdenv.isDarwin {
        extraOptions = {
          "UseKeychain" = "yes";
        };
      };
      "djm.ovh" = {
        hostname = "v.djm.ovh";
        port = 2222;
      };
      "devio" = {
        hostname = "devio.us";
        user = "deejayem";
        port = 2222;
      };
      "sdf" = {
        hostname = "sdf.org";
        user = "deejayem";
      };
      "sdfeu" = {
        hostname = "sdf-eu.org";
        user = "deejayem";
      };
      "grex" = {
        hostname = "grex.org";
        user = "deejayem";
      };
      "blinkenshell" = {
        hostname = "ssh.blinkenshell.org";
        port = 2222;
      };
      "hashbang" = {
        hostname = "de1.hashbang.sh";
      };
      "tilde.institute" = {
        hostname = "tilde.institute";
      };
      "tilde.team" = {
        hostname = "tilde.team";
      };
      "ctrl-c.club" = {
        hostname = "ctrl-c.club";
      };
      "github.com" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_ed25519";
        identitiesOnly = true;
      };
      "hb-backup" = {
        hostname = "de1.hashbang.sh";
        identityFile = "~/.ssh/hb_backup_key";
        identitiesOnly = true;
      };
      "bs-backup" = {
        hostname = "ssh.blinkenshell.org";
        port = 2222;
        identityFile = "~/.ssh/bs_backup_key";
        identitiesOnly = true;
      };
      "tt-backup" = {
        hostname = "tilde.team";
        identityFile = "~/.ssh/tt_backup_key";
        identitiesOnly = true;
      };
    };
  };
}
