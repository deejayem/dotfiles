{ pkgs }:
(pkgs.nix-plugins.override {
  nixComponents = pkgs.nixVersions.nixComponents_2_31;
}).overrideAttrs (old: {
  # Workaround: include extra-builtins.nix in nix-plugins package; see comment in ./default.nix
  postInstall = (old.postInstall or "") + ''
    mkdir -p $out/share/nix-plugins
    cp ${./extra-builtins.nix} $out/share/nix-plugins/extra-builtins.nix
  '';
})
