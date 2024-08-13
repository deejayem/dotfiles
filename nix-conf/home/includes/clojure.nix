{ config, pkgs, lib, ... }:
let
  inherit (lib) optionals;
in
{
  home.packages = with pkgs; [
    babashka
    clj-kondo
    clojure
    clojure-lsp
    jet
    maven
    neil
  ]
  ++ optionals stdenv.isLinux [ leiningen ];
}

