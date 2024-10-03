{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (lib) optionals;
in
{
  home.packages =
    with pkgs;
    [
      babashka
      clj-kondo
      clojure-lsp
      jet
      maven
      neil
    ]
    # TODO these are here because of the custom versions in otm.nix
    # but there should be a better way
    ++ optionals stdenv.isLinux [ leiningen clojure ];
}
