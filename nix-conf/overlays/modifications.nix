{ ... }:
final: prev:
let
  v = import ./_version-overrides.nix;
in
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
  direnv = prev.direnv.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or [ ]) ++ [
      ./direnv-workdir.patch
    ];
  });
  pythonPackagesExtensions =
    (prev.pythonPackagesExtensions or [ ])
    ++ prev.lib.optionals prev.stdenv.isDarwin [
      (
        _: python-prev:
        prev.lib.optionalAttrs (python-prev ? "cli-helpers") {
          "cli-helpers" = python-prev."cli-helpers".overridePythonAttrs (_: {
            doCheck = false;
          });
        }
      )
    ];
}
// prev.lib.optionalAttrs (prev.stdenv.isDarwin && prev.stdenv.isAarch64) {
  brave = prev.brave.overrideAttrs (
    finalAttrs: _: {
      inherit (v.brave) version;
      src = prev.fetchurl {
        url = "https://github.com/brave/brave-browser/releases/download/v${finalAttrs.version}/brave-v${finalAttrs.version}-darwin-arm64.zip";
        inherit (v.brave) hash;
      };
    }
  );
  google-chrome = prev.google-chrome.overrideAttrs (
    finalAttrs: _: {
      inherit (v.google-chrome) version slug;
      src = prev.fetchurl {
        url = "http://dl.google.com/release2/chrome/${finalAttrs.slug}_${finalAttrs.version}/GoogleChrome-${finalAttrs.version}.dmg";
        inherit (v.google-chrome) hash;
      };
    }
  );
  orbstack = prev.orbstack.overrideAttrs (
    finalAttrs: _: {
      inherit (v.orbstack) version;
      src = prev.fetchurl {
        url = "https://cdn-updates.orbstack.dev/arm64/OrbStack_v${
          prev.lib.replaceString "-" "_" finalAttrs.version
        }_arm64.dmg";
        inherit (v.orbstack) hash;
      };
    }
  );
  slack = prev.slack.overrideAttrs (
    finalAttrs: _: {
      inherit (v.slack) version;
      src = prev.fetchurl {
        url = "https://downloads.slack-edge.com/desktop-releases/mac/arm64/${finalAttrs.version}/Slack-${finalAttrs.version}-macOS.dmg";
        inherit (v.slack) hash;
      };
    }
  );
  zoom-us = prev.zoom-us.overrideAttrs (
    finalAttrs: _: {
      inherit (v.zoom-us) version;
      src = prev.fetchurl {
        url = "https://zoom.us/client/${finalAttrs.version}/zoomusInstallerFull.pkg?archType=arm64";
        name = "zoomusInstallerFull.pkg";
        inherit (v.zoom-us) hash;
      };
    }
  );
}
// prev.lib.optionalAttrs prev.stdenv.isDarwin {
  firefox-unwrapped =
    import (prev.path + "/pkgs/applications/networking/browsers/firefox/packages/firefox.nix")
      {
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
              version = v.firefox.version;
              packageVersion = v.firefox.version;
              src = prev.fetchurl {
                url = "mirror://mozilla/firefox/releases/${v.firefox.version}/source/firefox-${v.firefox.version}.source.tar.xz";
                inherit (v.firefox) sha512;
              };
            }
          );
      };
  firefox = final.wrapFirefox final.firefox-unwrapped { };
  vscode = prev.vscode.overrideAttrs (_: {
    # Work round preFixup/glibc/darwin issue
    preFixup = "";
  });
}
