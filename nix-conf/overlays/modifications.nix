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

  jiratui = prev.jiratui.overridePythonAttrs (oldAttrs: {
    postPatch = (oldAttrs.postPatch or "") + ''
      substituteInPlace pyproject.toml \
        --replace-fail "uv_build>=0.9.2,<0.10.0" uv_build
    '';
  });

} // prev.lib.optionalAttrs (prev.stdenv.isDarwin && prev.stdenv.isAarch64) {
  slack = prev.slack.overrideAttrs (finalAttrs: _: {
    version = "4.48.100";
    src = prev.fetchurl {
      url = "https://downloads.slack-edge.com/desktop-releases/mac/arm64/${finalAttrs.version}/Slack-${finalAttrs.version}-macOS.dmg";
      hash = "sha256-vzgxVBRncNQ4mchSgbe9vm3kEiPXHeMlhm3Xq4COi7A=";
    };
  });
  zoom-us = prev.zoom-us.overrideAttrs (finalAttrs: _: {
    version = "6.7.7.76486";
    src = prev.fetchurl {
      url = "https://zoom.us/client/${finalAttrs.version}/zoomusInstallerFull.pkg?archType=arm64";
      name = "zoomusInstallerFull.pkg";
      hash = "sha256-Wl8nghOfwGYxwzVjCScMeuiowhKLPYV04cagOpnzGUs=";
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
