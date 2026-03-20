{ ... }:
final: prev:
{
  atuin = prev.atuin.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or [ ]) ++ [
      ./atuin-session-host-filter.patch
    ];
  });
  babashka = prev.babashka.override {
    clojureToolsBabashka =
      let
        version = "1.12.4.1597";
      in
      prev.babashka.clojure-tools.overrideAttrs (_: {
        inherit version;
        src = prev.fetchurl {
          url = "https://github.com/clojure/brew-install/releases/download/${version}/clojure-tools-${version}.tar.gz";
          hash = "sha256-DgEvXVExaexDTLoonh/fVS5nHjgekL6BlFYLM9X6wkM=";
        };
      });
  };
  babashka-unwrapped = prev.babashka-unwrapped.overrideAttrs (
    finalAttrs: _: {
      version = "1.12.217";
      src = prev.fetchurl {
        url = "https://github.com/babashka/babashka/releases/download/v${finalAttrs.version}/babashka-${finalAttrs.version}-standalone.jar";
        sha256 = "sha256-5Nnzx2chre+h0SnM5spwiR9r4gjlyfc2FbgYa0spM34=";
      };
    }
  );
}
// prev.lib.optionalAttrs (prev.stdenv.isDarwin && prev.stdenv.isAarch64) {
  brave = prev.brave.overrideAttrs (
    finalAttrs: _: {
      version = "1.88.134";
      src = prev.fetchurl {
        url = "https://github.com/brave/brave-browser/releases/download/v${finalAttrs.version}/brave-v${finalAttrs.version}-darwin-arm64.zip";
        hash = "sha256-Q3wpSLj7yUPkaua01sMvShVFaJ4m0KTN+i7/zSS4WVw=";
      };
    }
  );
  google-chrome = prev.google-chrome.overrideAttrs (
    finalAttrs: _: {
      version = "146.0.7680.154";
      slug = "aduhru4wjcwjo2cuql7gnsdev6hq";
      src = prev.fetchurl {
        url = "http://dl.google.com/release2/chrome/${finalAttrs.slug}_${finalAttrs.version}/GoogleChrome-${finalAttrs.version}.dmg";
        hash = "sha256-u/i8fYn53BbQGFlBFTEayNpSQoeNPBJEBXr2KFArgW8=";
      };
    }
  );
  slack = prev.slack.overrideAttrs (
    finalAttrs: _: {
      version = "4.48.102";
      src = prev.fetchurl {
        url = "https://downloads.slack-edge.com/desktop-releases/mac/arm64/${finalAttrs.version}/Slack-${finalAttrs.version}-macOS.dmg";
        hash = "sha256-mnoNnyv6sU6WLiPsWWPuUL686MQ/vxnGokfdWOSO8b4=";
      };
    }
  );
  zoom-us = prev.zoom-us.overrideAttrs (
    finalAttrs: _: {
      version = "6.7.7.76486";
      src = prev.fetchurl {
        url = "https://zoom.us/client/${finalAttrs.version}/zoomusInstallerFull.pkg?archType=arm64";
        name = "zoomusInstallerFull.pkg";
        hash = "sha256-Wl8nghOfwGYxwzVjCScMeuiowhKLPYV04cagOpnzGUs=";
      };
    }
  );
}
// prev.lib.optionalAttrs prev.stdenv.isDarwin {
  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev: {
      warp = prev.haskell.lib.dontCheck hprev.warp;
    };
  };
  less = prev.less.overrideAttrs (
    finalAttrs: _: {
      version = "692";
      src = prev.fetchurl {
        url = "https://www.greenwoodsoftware.com/less/less-${finalAttrs.version}.tar.gz";
        hash = "sha256-YTAPYDeY7PHXeGVweJ8P8/WhrPB1pvufdWg30WbjfRQ=";
      };
    }
  );
}
