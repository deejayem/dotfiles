{ ... }:
final: prev:
{
  atuin = prev.atuin.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or [ ]) ++ [
      (
        if prev.lib.versionAtLeast prev.atuin.version "18.13.0" then
          ./atuin-session-host-filter-unstable.patch
        else
          ./atuin-session-host-filter.patch
      )
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
      version = "1.88.136";
      src = prev.fetchurl {
        url = "https://github.com/brave/brave-browser/releases/download/v${finalAttrs.version}/brave-v${finalAttrs.version}-darwin-arm64.zip";
        hash = "sha256-Jf/dQy8o87iytsWcTUpPlYTOcdvnzJjlkH1M6y+x9Dw=";
      };
    }
  );
  google-chrome = prev.google-chrome.overrideAttrs (
    finalAttrs: _: {
      version = "146.0.7680.178";
      slug = "acieaz5gurxr6um2wu2e5hogjueq";
      src = prev.fetchurl {
        url = "http://dl.google.com/release2/chrome/${finalAttrs.slug}_${finalAttrs.version}/GoogleChrome-${finalAttrs.version}.dmg";
        hash = "sha256-aGqEFAzQZLy85hbsjhgYr5eFYgCaMhOiUG00wSlANHk=";
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
      version = "7.0.0.77593";
      src = prev.fetchurl {
        url = "https://zoom.us/client/${finalAttrs.version}/zoomusInstallerFull.pkg?archType=arm64";
        name = "zoomusInstallerFull.pkg";
        hash = "sha256-YSUaM8YAJHigm4M9W34/bD164M8f/hbhtcmHyUwFN20=";
      };
    }
  );
}
// prev.lib.optionalAttrs prev.stdenv.isDarwin {
  firefox-unwrapped =
    let
      version = "149.0";
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
              sha512 = "0d3g59b04xnrjmc75zq53d4csdwj1vys7nfc8b8srcvn1k3fig6g6f14pxcpb823682i6sc9cc49gyifi49rs03ly2hvdwgffkp3n6d";
            };
          }
        );
    };
  firefox = final.wrapFirefox final.firefox-unwrapped { };
}
