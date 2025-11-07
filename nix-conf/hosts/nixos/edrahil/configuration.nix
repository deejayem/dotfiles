{ config, inputs, ... }:
let
  private = import ./private.nix;
in
{
  _module.args = { inherit private; };

  imports = [
    ./hardware-configuration.nix
    ./network-configuration.nix
    ../modules/base.nix
    inputs.sops-nix.nixosModules.sops
  ];

  networking.hostName = "edrahil";

  networking.firewall.allowedTCPPorts = [ 2222 ];

  services.openssh.ports = [ 2222 ];
  services.openssh.allowSFTP = true;

  sops = {
    defaultSopsFile = builtins.path {
      path = ./secrets.yaml;
      name = "${config.networking.hostName}-secrets.yaml";
    };
    secrets.restic_password = {
      owner = config.users.users.djm.name;
    };
  };

  services.restic = {
    backups = {
      hb = {
        paths = [ "${config.users.users.djm.home}" ];
        repository = "sftp:djm@hb-backup:/home/djm/backup/edrahil";
        initialize = true;
        user = "djm";
        environmentFile = "/etc/restic-environment";
        passwordFile = config.sops.secrets.restic_password.path;
        timerConfig = {
          OnCalendar = "02:25";
          RandomizedDelaySec = "20min";
        };
        exclude = [
          "irclogs"
          ".cache"
          ".config"
          ".directory_history"
          ".local"
          "nixpkgs"
        ];
        extraBackupArgs = [
          "--compression=max"
        ];
        pruneOpts = [
          "--keep-daily 5"
          "--keep-weekly 2"
          "--keep-monthly 3"
        ];
      };
      bs = {
        paths = [ "${config.users.users.djm.home}" ];
        repository = "sftp:djm@bs-backup:/home/djm/backup/edrahil";
        initialize = true;
        user = "djm";
        environmentFile = "/etc/restic-environment";
        passwordFile = config.sops.secrets.restic_password.path;
        timerConfig = {
          OnCalendar = "03:15";
          RandomizedDelaySec = "20min";
        };
        exclude = [
          "irclogs"
          ".cache"
          ".config"
          ".directory_history"
          ".local"
          "nixpkgs"
        ];
        extraBackupArgs = [
          "--compression=max"
        ];
        pruneOpts = [
          "--keep-daily 5"
          "--keep-weekly 2"
          "--keep-monthly 3"
        ];
      };
      tt = {
        paths = [ "${config.users.users.djm.home}" ];
        repository = "sftp:djm@tt-backup:/home/djm/backup/edrahil";
        initialize = true;
        user = "djm";
        environmentFile = "/etc/restic-environment";
        passwordFile = config.sops.secrets.restic_password.path;
        timerConfig = {
          OnCalendar = "04:05";
          RandomizedDelaySec = "20min";
        };
        exclude = [
          "irclogs"
          ".cache"
          ".config"
          ".directory_history"
          ".local"
          "nixpkgs"
        ];
        extraBackupArgs = [
          "--compression=max"
        ];
        pruneOpts = [
          "--keep-daily 5"
          "--keep-weekly 2"
          "--keep-monthly 3"
        ];
      };
    };
  };

  environment.etc = {
    "restic-environment" = {
      text = ''
        RESTIC_COMPRESSION=max
      '';
    };
  };

  system.stateVersion = "22.05";
}
