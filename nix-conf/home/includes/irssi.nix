{ config, pkgs, lib, ... }:

{
  programs.irssi = {
    enable = true;
    aliases = {
      WINC = "window close";
      WII = "whois $0 $0";
      J = "join";
      W = "who";
      N = "names";
      CL = "clear";
      K = "kick";
      KB = "kickban";
      BANS = "ban";
      SB = "scrollback";
      UMODE = "mode $N";
      WC = "window close";
      GOTO = "sb goto";
      "1" = "window 1";
      "2" = "window 2";
      "3" = "window 3";
      "4" = "window 4";
      "5" = "window 5";
      "6" = "window 6";
      "7" = "window 7";
      "8" = "window 8";
      "9" = "window 9";
      "10" = "window 10";
      "11" = "window 11";
      "12" = "window 12";
      "13" = "window 13";
      "14" = "window 14";
      "15" = "window 15";
      "16" = "window 16";
      "17" = "window 17";
      "18" = "window 18";
      "19" = "window 19";
      "20" = "window 20";
    };
    extraConfig = ''
      settings = {
        core = {
          real_name = "David Morgan";
          user_name = "djm";
          nick = "djm";
          timestamp_format = "%H:%M:%S";
        };
        "irc/core" = { usermode = "+iwR"; };
        "fe-common/core" = {
          autolog_path = "~/irclogs/%Y/$tag/$0.%m-%d.log";
          autolog = "yes";
          #beep_msg_level = "hilight msgs";
          beep_when_away = "yes";
          #activity_hide_level = "none";
          #theme = "default";
          hide_colors = "yes";
        };
        "fe-text" = {
          paste_verify_line_count = "1";
          actlist_sort = "refnum";
          paste_join_multiline = "yes";
        };
        hilights = (
          {
            text = "djm";
            nick = "yes";
            word = "yes";
            fullword = "yes";
            matchcase = "yes";
          }
        );
      }
      windows = {
        1 = { immortal = "yes"; name = "(status)"; level = "ALL"; };
        2 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#institute"; tag = "tilde"; }); };
        3 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#meta"; tag = "tilde"; }); };
        4 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#blinkenshell"; tag = "blinkenirc"; }); };
        5 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#blinkenshell.op"; tag = "blinkenirc"; }); };
        6 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#prosapologian"; tag = "refchat"; }); };
        7 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#clojure"; tag = "libera"; }); };
        8 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#emacs"; tag = "libera"; }); };
        9 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#emacs"; tag = "tilde"; }); };
        10 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#ctrl-c"; tag = "tilde"; }); };
        11 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#team"; tag = "tilde"; }); };
        12 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#systemcrafters"; tag = "libera"; }); };
        13 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#!"; tag = "hashbang"; }); };
        14 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#nixos"; tag = "libera"; }); };
        15 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#home-manager"; tag = "oftc"; }); };
        16 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "&bitlbee"; tag = "bitlbee"; }); };
        17 = { items = ( { type = "CHANNEL"; chat_type = "IRC"; name = "#twitter_deejayem"; tag = "bitlbee"; }); };
      };
    '';
    networks = {
      libera = {
        nick = "djm";
        saslExternal = true;
        server = {
          address = "irc.libera.chat";
          port = 6697;
          autoConnect = true;
          ssl = {
            enable = true;
            verify = false;
            certificateFile = "${config.home.homeDirectory}/.irssi/libera.pem";
          };
        };
        channels = {
          clojure.autoJoin = true;
          emacs.autoJoin = true;
          nixos.autoJoin = true;
          systemcrafters.autoJoin = true;
        };
      };
      tilde = {
        nick = "djm";
        saslExternal = true;
        server = {
          address = "irc.tilde.chat";
          port = 6697;
          autoConnect = true;
          ssl = {
            enable = true;
            verify = true;
            certificateFile = "${config.home.homeDirectory}/.irssi/tilde.pem";
          };
        };
        channels = {
          ctrl-c.autoJoin = true;
          emacs.autoJoin = true;
          institute.autoJoin = true;
          meta.autoJoin = true;
          team.autoJoin = true;
        };
      };
      oftc = {
        nick = "djm";
        server = {
          address = "irc.oftc.net";
          port = 6697;
          autoConnect = true;
          ssl = {
            enable = true;
            verify = true;
            certificateFile = "${config.home.homeDirectory}/.irssi/oftc.pem";
          };
        };
        channels = {
          home-manager.autoJoin = true;
        };
      };
      blinkenirc = {
        nick = "djm";
        autoCommands = [ "msg chanserv invite #blinkenshell.op" "wait 2000" ];
        saslExternal = true;
        server = {
          address = "irc.blinkenshell.org";
          port = 6697;
          autoConnect = true;
          ssl = {
            enable = true;
            verify = true;
            certificateFile = "${config.home.homeDirectory}/.irssi/blinkenirc.pem";
          };
        };
        channels = {
          blinkenshell.autoJoin = true;
          "blinkenshell.op".autoJoin = true;
        };
      };
      hashbang = {
        nick = "djm";
        saslExternal = true;
        server = {
          address = "irc.hashbang.sh";
          port = 6697;
          autoConnect = true;
          ssl = {
            enable = true;
            verify = true;
            certificateFile = "${config.home.homeDirectory}/.irssi/hashbang.pem";
          };
        };
        channels = {
          "#!".autoJoin = true;
        };
      };
      refchat = {
        nick = "djm";
        server = {
          address = "gill.refchat.net";
          port = 6697;
          autoConnect = true;
          ssl = {
            enable = true;
            verify = false;
            certificateFile = "${config.home.homeDirectory}/.irssi/refchat.pem";
          };
        };
        channels = {
          prosapologian.autoJoin = true;
        };
      };
      bitlbee = {
        nick = "djm";
        autoCommands = [ "bitlbee_identify" ];
        server = {
          address = "testing.bitlbee.org";
          port = 6697;
          autoConnect = true;
          ssl = {
            enable = true;
          };
        };
      };
    };
  };
}
