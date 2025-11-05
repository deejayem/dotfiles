{ pkgs, lib, ... }:
{
  services.greetd = {
    enable = true;
    settings = {
      default_session.command = ''
        ${lib.getExe pkgs.greetd.tuigreet} \
          --time \
          --asterisks \
          --user-menu \
          --cmd sway
      '';
    };
    vt = 2;
  };

  environment.etc."greetd/environments".text = ''
    sway
  '';
}
