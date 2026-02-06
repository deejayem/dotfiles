{ pkgs, ... }:
{
  imports = [
    ../programs/clojure.nix
  ];

  home.packages = with pkgs; [
    docker
    docker-compose
    docker-credential-helpers
    gopass-jsonapi
    imagemagick
    janet
    multimarkdown
    nixd
  ];
}
