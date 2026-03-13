{ ... }:
final: prev:
{
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

}
// prev.lib.optionalAttrs (prev.stdenv.isDarwin && prev.stdenv.isAarch64) {
  brave = prev.brave.overrideAttrs (
    finalAttrs: _: {
      version = "1.88.127";
      src = prev.fetchurl {
        url = "https://github.com/brave/brave-browser/releases/download/v${finalAttrs.version}/brave-v${finalAttrs.version}-darwin-arm64.zip";
        hash = "sha256-GNz1O145cKSSWVMl02PtpXHQjtDblxyAnYFqhopd8qQ=";
      };
    }
  );
  google-chrome = prev.google-chrome.overrideAttrs (
    finalAttrs: _: {
      version = "146.0.7680.76";
      slug = "adgzhtm53eqdw4h4wn64ebox7o6q";
      src = prev.fetchurl {
        url = "http://dl.google.com/release2/chrome/${finalAttrs.slug}_${finalAttrs.version}/GoogleChrome-${finalAttrs.version}.dmg";
        hash = "sha256-nC8y6992wlx6DcN48glkeoZFSze1vNkbsqENmqC5nrQ=";
      };
    }
  );
  slack = prev.slack.overrideAttrs (
    finalAttrs: _: {
      version = "4.48.100";
      src = prev.fetchurl {
        url = "https://downloads.slack-edge.com/desktop-releases/mac/arm64/${finalAttrs.version}/Slack-${finalAttrs.version}-macOS.dmg";
        hash = "sha256-vzgxVBRncNQ4mchSgbe9vm3kEiPXHeMlhm3Xq4COi7A=";
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
  firefox-unwrapped =
    let
      version = "148.0.2";
    in
    import (prev.path + "/pkgs/applications/networking/browsers/firefox/packages/firefox.nix") {
      inherit (final)
        stdenv
        lib
        callPackage
        fetchurl
        nixosTests
        ;
      buildMozillaMach =
        args:
        final.buildMozillaMach (
          args
          // {
            inherit version;
            packageVersion = version;
            src = prev.fetchurl {
              url = "mirror://mozilla/firefox/releases/${version}/source/firefox-${version}.source.tar.xz";
              sha512 = "3phncmlfr8pclmxxi83vqzsrdnbgwdfxw4929lg337l3apsvrci83igp8cvjbr02357qyfvy773ppfsmmnj9djn5x7p0f2z4gjkmaan";
            };
          }
        );
    };
  firefox = final.wrapFirefox final.firefox-unwrapped { };
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
