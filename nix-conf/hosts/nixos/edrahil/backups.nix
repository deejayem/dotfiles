{ config, pkgs, ... }:

let
  commonConfig = {
    paths = [ "${config.users.users.djm.home}" ];
    initialize = true;
    user = "djm";
    environmentFile = "/etc/restic-environment";
    passwordFile = config.age.secrets."restic/password".path;
    exclude = [
      "irclogs"
      ".cache"
      ".config"
      ".directory_history"
      ".local"
      "nixpkgs"
    ];
    extraBackupArgs = [ "--compression=max" ];
    pruneOpts = [
      "--keep-daily 5"
      "--keep-weekly 2"
      "--keep-monthly 3"
    ];
  };
  mkBackup =
    host: time:
    commonConfig
    // {
      repository = "sftp:djm@${host}-backup:/home/djm/backup/edrahil";
      timerConfig = {
        OnCalendar = time;
        RandomizedDelaySec = "20min";
      };
    };
in
{
  age.secrets."restic/password".owner = config.users.users.djm.name;

  services.restic.backups = {
    hb = mkBackup "hb" "02:25";
    bs = mkBackup "bs" "03:15";
    tt = mkBackup "tt" "04:05";
  };

  environment.etc = {
    "restic-environment" = {
      text = ''
        RESTIC_COMPRESSION=max
      '';
    };
  };
}
