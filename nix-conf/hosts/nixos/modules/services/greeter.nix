{ pkgs, lib, ... }:
{
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = ''
          ${lib.getExe pkgs.tuigreet} \
            --time \
            --asterisks \
            --remember \
            --cmd sway
        '';
      };
    };
  };

  environment.etc."greetd/environments".text = ''
    sway
  '';

  # Prevent boot messages from messing up tuigreet
  # (https://github.com/sjcobb2022/nixos-config/blob/720ffb2c9c7f3643460de5f31bcca30ec38c1ebe/hosts/common/optional/greetd.nix)
  systemd.services.greetd.serviceConfig = {
    Type = "idle";
    StandardInput = "tty";
    StandardOutput = "tty";
    StandardError = "journal";
    TTYReset = true;
    TTYVHangup = true;
    TTYVTDisallocate = true;
  };
}
