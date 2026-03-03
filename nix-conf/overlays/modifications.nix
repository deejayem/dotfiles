{ ... }:
final: prev: {
  atuin = prev.atuin.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or [ ]) ++ [
      ./atuin-session-host-filter.patch
    ];
  });

  firebase-tools = prev.firebase-tools.override {
    buildNpmPackage = prev.buildNpmPackage.override {
      nodejs = prev.nodejs_22;
    };
  };

  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev: {
      warp = prev.haskell.lib.dontCheck hprev.warp;
    };
  };
  less = prev.less.overrideAttrs (finalAttrs: _: {
    version = "692";
    src = prev.fetchurl {
      url = "https://www.greenwoodsoftware.com/less/less-${finalAttrs.version}.tar.gz";
      hash = "sha256-YTAPYDeY7PHXeGVweJ8P8/WhrPB1pvufdWg30WbjfRQ=";
    };
  });
}
