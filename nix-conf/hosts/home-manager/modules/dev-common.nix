{ pkgs, lib, ... }:
let
  inherit (lib) optionals;
in
{
  imports = [
    ./common.nix
    ./programs/clojure.nix
  ];

  home.packages =
    with pkgs;
    [
      docker
      docker-compose
      docker-credential-helpers
      gopass-jsonapi
      imagemagick
      janet
      multimarkdown
      nixd
      ripgrep-all

      unstable.mcp-nixos
    ]
    ++ optionals (!stdenv.isDarwin) [
      ffmpeg
      mpv
    ];
}
