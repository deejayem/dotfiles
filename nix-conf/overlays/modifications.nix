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

} // prev.lib.optionalAttrs (prev.stdenv.isDarwin && prev.stdenv.isAarch64) {
  slack = prev.slack.overrideAttrs (finalAttrs: _: {
    version = "4.48.100";
    src = prev.fetchurl {
      url = "https://downloads.slack-edge.com/desktop-releases/mac/arm64/${finalAttrs.version}/Slack-${finalAttrs.version}-macOS.dmg";
      hash = "sha256-vzgxVBRncNQ4mchSgbe9vm3kEiPXHeMlhm3Xq4COi7A=";
    };
  });
} // prev.lib.optionalAttrs prev.stdenv.isDarwin {
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
