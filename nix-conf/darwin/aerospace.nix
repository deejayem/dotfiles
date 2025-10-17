{
  services.aerospace = {
    enable = false;
    settings =
      let
        mod = "cmd-alt-ctrl-shift";
      in
      {
        workspace-to-monitor-force-assignment = {
          "1" = "secondary"; # Emacs
          "2" = "main"; # YouTube Music
          "3" = "main"; # Chrome
          "4" = "main"; # Firefox
          "5" = "main"; # Slack
          "6" = "main"; # Alacritty
          "7" = "main"; # Zoom
          "8" = "main"; # OrbStack
          "9" = "main"; # Others
          "0" = "secondary"; # Others
        };

        on-window-detected = [
          {
            "if".app-id = "org.gnu.Emacs";
            run = [ "move-node-to-workspace 1" ];
          }
          {
            "if".app-id = "org.gnu.Emacs";
            run = [ "layout floating" ];
          }
          {
            "if".app-id = "com.github.th-ch.youtube-music";
            run = [ "move-node-to-workspace 2" ];
          }
          {
            "if".app-id = "com.google.Chrome";
            run = [ "move-node-to-workspace 3" ];
          }
          {
            "if".app-id = "org.mozilla.firefox";
            run = [ "move-node-to-workspace 4" ];
          }
          {
            "if".app-id = "com.tinyspeck.slackmacgui";
            run = [ "move-node-to-workspace 5" ];
          }
          {
            "if".app-id = "org.alacritty";
            run = [ "move-node-to-workspace 6" ];
          }
          {
            "if".app-id = "us.zoom.xos";
            run = [ "move-node-to-workspace 7" ];
          }
          {
            "if".app-id = "com.orbstack.OrbStack";
            run = [ "move-node-to-workspace 8" ];
          }
        ];

        mode.main.binding = {
          "${mod}-h" = "focus left";
          "${mod}-j" = "focus down";
          "${mod}-k" = "focus up";
          "${mod}-l" = "focus right";

          "${mod}-shift-h" = "move left";
          "${mod}-shift-j" = "move down";
          "${mod}-shift-k" = "move up";
          "${mod}-shift-l" = "move right";

          "${mod}-comma" = "focus-monitor --wrap-around prev";
          "${mod}-period" = "focus-monitor --wrap-around next";

          "${mod}-shift-comma" = "move-node-to-monitor --wrap-around prev";
          "${mod}-shift-period" = "move-node-to-monitor --wrap-around next";

          "${mod}-1" = "workspace 1"; # Emacs
          "${mod}-2" = "workspace 2"; # YouTube Music
          "${mod}-3" = "workspace 3"; # Chrome
          "${mod}-4" = "workspace 4"; # Firefox
          "${mod}-5" = "workspace 5"; # Slack
          "${mod}-6" = "workspace 6"; # Alacritty
          "${mod}-7" = "workspace 7"; # Zoom
          "${mod}-8" = "workspace 8"; # OrbStack
          "${mod}-9" = "workspace 9"; # Others
          "${mod}-0" = "workspace 0"; # Others

          "${mod}-shift-1" = "move-node-to-workspace 1";
          "${mod}-shift-2" = "move-node-to-workspace 2";
          "${mod}-shift-3" = "move-node-to-workspace 3";
          "${mod}-shift-4" = "move-node-to-workspace 4";
          "${mod}-shift-5" = "move-node-to-workspace 5";
          "${mod}-shift-6" = "move-node-to-workspace 6";
          "${mod}-shift-7" = "move-node-to-workspace 7";
          "${mod}-shift-8" = "move-node-to-workspace 8";
          "${mod}-shift-9" = "move-node-to-workspace 9";
          "${mod}-shift-0" = "move-node-to-workspace 0";

          "${mod}-f" = "fullscreen";
          "${mod}-shift-f" = "layout floating tiling";
          "${mod}-r" = "mode resize";

          "${mod}-v" = "join-with down";
          "${mod}-s" = "join-with right";

          "${mod}-shift-r" = "reload-config";
        };

        mode.resize.binding = {
          "h" = "resize width -50";
          "j" = "resize height +50";
          "k" = "resize height -50";
          "l" = "resize width +50";
          "enter" = "mode main";
          "esc" = "mode main";
        };
      };
  };
}
