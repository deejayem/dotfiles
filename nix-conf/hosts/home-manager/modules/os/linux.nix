{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../programs/gpg-agent.nix
    ../programs/keychain.nix
  ];

  home.packages =
    with pkgs;
    [
      libtree
      msmtp
      restic
      sword
      yt-dlp
    ]
    ++ lib.optionals (config.host.role == "workstation") [
      ffmpeg
      lame
      mp3cat
      mpv
    ];
}
