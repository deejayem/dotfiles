{
  config,
  pkgs,
  ...
}:
{
  home.packages =
    with pkgs;
    [
      babashka
      clj-kondo
      clojure
      clojure-lsp
      emacs-lsp-booster
      jet
      leiningen
      maven
      neil
    ];
}
