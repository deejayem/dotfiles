{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    networkmanagerapplet
    pavucontrol
  ];

  wayland.windowManager.sway = {
    enable = true;
    package = pkgs.sway;

    config = {
      modifier = "Mod4";
      terminal = "${pkgs.foot}/bin/foot";

      fonts = {
        names = [ "monospace" ];
        size = 8.0;
      };

      input = {
        "*" = {
          xkb_layout = "gb";
        };
        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
          click_method = "clickfinger";
          # middle_emulation = "enabled"; # TODO is this wanted?'
        };
      };

      keybindings =
        let
          mod = config.wayland.windowManager.sway.config.modifier;
        in
        lib.mkOptionDefault {
          "${mod}+Return" = "exec ${lib.getExe pkgs.foot}";
          "${mod}+d" = "exec ${lib.getExe pkgs.rofi} -modi drun -show drun";
          "${mod}+p" = "exec ${lib.getExe pkgs.playerctl} play-pause";
          "${mod}+o" = "move workspace to output right";
        };

      output = {
        "eDP-1" = {
          mode = "1920x1080@60Hz";
          position = "0,0";
        };
        "HDMI-A-1" = {
          mode = "2560x1440@60Hz";
          position = "1920,0";
        };
      };

      assigns = {
        "2" = [ { app_id = "firefox"; } ];
        "3" = [ { app_id = "emacs"; } ];
        "4" = [ { app_id = "chromium-browser"; } ];
      };

      bars = [
        {
          command = "waybar";
          position = "top";
        }
      ];

      startup = [
        { command = "${pkgs.dex}/bin/dex --autostart --environment sway"; }
        { command = "${pkgs.networkmanagerapplet}/bin/nm-applet"; }
        {
          command = "${pkgs.swayidle}/bin/swayidle -w timeout 300 '${pkgs.swaylock}/bin/swaylock -f' timeout 600 'swaymsg \"output * power off\"' resume 'swaymsg \"output * power on\"'";
        }
        { command = "swaymsg workspace number 1"; }
        { command = "${lib.getExe pkgs.sway-audio-idle-inhibit}"; }
      ];
    };
  };

  programs.waybar = {
    enable = true;
    style = ''
      * {
        border: none;
        border-radius: 0;
        font-family: "JetBrainsMono Nerd Font";
        font-size: 13px;
        min-height: 0;
      }

      window#waybar {
        background: #1a1a1a;
        color: #c0c0c0;
      }

      #workspaces button,
      #clock,
      #battery,
      #cpu,
      #memory,
      #network,
      #pulseaudio,
      #tray,
      #temperature,
      #keyboard-state,
      #idle_inhibitor,
      #language,
      #submap,
      #backlight,
      #custom-backlight,
      #brightness {
        padding: 0 10px;
        background: transparent;
        color: #c0c0c0;
      }

      #workspaces button.focused {
        background: #2a2a2a;
        color: #ffffff;
      }

      #battery.charging,
      #battery.warning:not(.charging) {
        color: #888888;
      }

      #battery.critical:not(.charging) {
        color: #666666;
      }

      #temperature.critical {
        color: #c0c0c0;
      }

      #keyboard-state > label {
        padding: 0 5px;
        color: #c0c0c0;
      }

      #keyboard-state > label.locked {
        background: transparent;
      }
  '';
  };

  programs.foot = {
    enable = true;
    settings = {
      main.font = "MesloLGS NF:size=8";
      colors = {
        background = "000000";
        foreground = "bbbbbb";
        regular0 = "000000"; # black
        regular1 = "cc0403"; # red
        regular2 = "19cb00"; # green
        regular3 = "cecb00"; # yellow
        regular4 = "3366ff"; # blue
        regular5 = "cb1ed1"; # magenta
        regular6 = "0dcdcd"; # cyan
        regular7 = "e5e5e5"; # white
        bright0 = "4d4d4d"; # bright black
        bright1 = "3e0605"; # bright red
        bright2 = "23fd00"; # bright green
        bright3 = "fffd00"; # bright yellow
        bright4 = "4080ff"; # bright blue
        bright5 = "fd28ff"; # bright magenta
        bright6 = "14ffff"; # bright cyan
        bright7 = "ffffff"; # bright white
      };
    };
  };
}
