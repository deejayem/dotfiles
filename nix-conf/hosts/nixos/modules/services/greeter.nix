{ pkgs, lib, ... }:
{
  services.greetd = {
    enable = true;
    settings = {
      default_session.command = ''
        ${lib.getExe pkgs.tuigreet} \
          --time \
          --asterisks \
          --user-menu \
          --cmd sway
      '';
    };
  };

  environment.etc."greetd/environments".text = ''
    sway
  '';
}
