{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkForce;

  nulPlaceholder = "__ALACRITTY_NUL__";
  esc = builtins.fromJSON ''"\u001b"'';
  controlChar = hex: builtins.fromJSON ''"\u00${hex}"'';

  commandToControlBindings =
    let
      ctrlChars = {
        A = controlChar "01";
        B = controlChar "02";
        C = controlChar "03";
        D = controlChar "04";
        E = controlChar "05";
        F = controlChar "06";
        G = controlChar "07";
        H = controlChar "08";
        I = controlChar "09";
        J = controlChar "0a";
        K = controlChar "0b";
        L = controlChar "0c";
        M = controlChar "0d";
        N = controlChar "0e";
        O = controlChar "0f";
        P = controlChar "10";
        Q = controlChar "11";
        R = controlChar "12";
        S = controlChar "13";
        T = controlChar "14";
        U = controlChar "15";
        V = controlChar "16";
        W = controlChar "17";
        X = controlChar "18";
        Y = controlChar "19";
        Z = controlChar "1a";
      };
    in
    lib.mapAttrsToList (key: chars: {
      inherit key chars;
      mods = "Command";
    }) ctrlChars;

  mkCtrlCsiU = key: codepoint: {
    inherit key;
    mods = "Command";
    chars = "${esc}[${toString codepoint};5u";
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
      key = "\\";
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
        {
          key = "Return";
          mods = "Shift";
          chars = "${esc}[13;2u";
        }
      ]
      # Swap ctrl and cmd as much as possible on darwin
      ++ lib.optionals pkgs.stdenv.isDarwin (
        commandToControlBindings
        ++ commandToControlCsiUBindings
        ++ [
          {
            key = "Space";
            mods = "Command";
            chars = nulPlaceholder;
          }
        ]
      );

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

  xdg.configFile."alacritty/alacritty.toml".source =
    let
      toml = pkgs.formats.toml { };
      generated = toml.generate "alacritty.toml" config.programs.alacritty.settings;
    in
    mkForce (
      pkgs.runCommand "alacritty.toml" { } ''
        cp ${generated} "$out"
        substituteInPlace "$out" \
          --replace-fail "'${nulPlaceholder}'" '"\u0000"'
      ''
    );
}
