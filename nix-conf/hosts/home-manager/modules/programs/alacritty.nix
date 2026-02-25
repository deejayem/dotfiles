{ lib, pkgs, ... }:
let
  commandToControlBindings =
    let
      ctrlChars = {
        A = "\\u0001";
        B = "\\u0002";
        C = "\\u0003";
        D = "\\u0004";
        E = "\\u0005";
        F = "\\u0006";
        G = "\\u0007";
        H = "\\u0008";
        I = "\\u0009";
        J = "\\u000a";
        K = "\\u000b";
        L = "\\u000c";
        M = "\\u000d";
        N = "\\u000e";
        O = "\\u000f";
        P = "\\u0010";
        Q = "\\u0011";
        R = "\\u0012";
        S = "\\u0013";
        T = "\\u0014";
        U = "\\u0015";
        V = "\\u0016";
        W = "\\u0017";
        X = "\\u0018";
        Y = "\\u0019";
        Z = "\\u001a";
      };
    in
    lib.mapAttrsToList (key: chars: {
      inherit key chars;
      mods = "Command";
    }) ctrlChars;

  mkCtrlCsiU = key: codepoint: {
    inherit key;
    mods = "Command";
    chars = "\\u001b[${toString codepoint};5u";
  };

  commandToControlCsiUBindings = map (v: mkCtrlCsiU v.key v.codepoint) [
    {
      key = "0";
      codepoint = 48;
    }
    {
      key = "1";
      codepoint = 49;
    }
    {
      key = "2";
      codepoint = 50;
    }
    {
      key = "3";
      codepoint = 51;
    }
    {
      key = "4";
      codepoint = 52;
    }
    {
      key = "5";
      codepoint = 53;
    }
    {
      key = "6";
      codepoint = 54;
    }
    {
      key = "7";
      codepoint = 55;
    }
    {
      key = "8";
      codepoint = 56;
    }
    {
      key = "9";
      codepoint = 57;
    }
    {
      key = ".";
      codepoint = 46;
    }
    {
      key = ",";
      codepoint = 44;
    }
    {
      key = "/";
      codepoint = 47;
    }
    {
      key = ";";
      codepoint = 59;
    }
    {
      key = "'";
      codepoint = 39;
    }
    {
      key = "-";
      codepoint = 45;
    }
    {
      key = "=";
      codepoint = 61;
    }
    {
      key = "[";
      codepoint = 91;
    }
    {
      key = "]";
      codepoint = 93;
    }
    {
      key = "\\\\";
      codepoint = 92;
    }
    {
      key = "`";
      codepoint = 96;
    }
  ];
in
{
  programs.alacritty = {
    enable = true;
    # On darwin we will install this with nix-darwin instead
    package = if pkgs.stdenv.isDarwin then null else pkgs.alacritty;
    settings = {

      terminal.shell = {
        program = "/run/current-system/sw/bin/zsh";
      };

      font = {
        normal.family = "MesloLGS NF";
        size = 12;
      };

      selection = {
        save_to_clipboard = true;
      };

      keyboard.bindings = [
        {
          key = "Insert";
          mods = "Shift";
          action = "Paste";
        }
      ]
      # Swap ctrl and cmd as much as possible on darwin
      ++ lib.optionals pkgs.stdenv.isDarwin (commandToControlBindings ++ commandToControlCsiUBindings ++ [
        {
          key = "Space";
          mods = "Command";
          chars = "\\u0000";
        }
      ]);

      window = lib.optionalAttrs pkgs.stdenv.isDarwin {
        option_as_alt = "OnlyLeft";
      };

      colors = {
        primary = {
          background = "#000000";
          foreground = "#c7c7c7";
        };

        cursor = {
          text = "#000000";
          cursor = "#c7c7c7";
        };

        selection = {
          text = "#000000";
          background = "#b5d5ff";
        };

        normal = {
          black = "#000000";
          red = "#c91b00";
          green = "#00c200";
          yellow = "#c7c400";
          blue = "#3366cc";
          magenta = "#ca30c7";
          cyan = "#00c5c7";
          white = "#c7c7c7";
        };

        bright = {
          black = "#686868";
          red = "#ff6e67";
          green = "#5ff967";
          yellow = "#fefb67";
          blue = "#6871ff";
          magenta = "#ff76ff";
          cyan = "#5ffdff";
          white = "#feffff";
        };
      };
    };
  };
}
