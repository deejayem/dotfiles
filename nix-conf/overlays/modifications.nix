{ ... }:
final: prev:
let
  v = import ./_version-overrides.nix;
in
{
  atuin = prev.atuin.overrideAttrs (oldAttrs: {
    __darwinAllowLocalNetworking = true;
    patches = (oldAttrs.patches or [ ]) ++ [
      (
        if prev.lib.versionAtLeast prev.atuin.version "18.19.0" then
          ./atuin-session-host-filter-unstable.patch
        else
          ./atuin-session-host-filter.patch
      )
    ];
    # The file watcher tests need FSEvents, which the darwin sandbox blocks
    sandboxProfile = (oldAttrs.sandboxProfile or "") + ''
      (allow mach-lookup (global-name "com.apple.FSEvents"))
    '';
  });
  direnv = prev.direnv.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or [ ]) ++ [
      (
        if prev.lib.versionAtLeast prev.direnv.version "2.38.0" then
          ./direnv-workdir-unstable.patch
        else
          ./direnv-workdir.patch
      )
    ];
  });
  git-remote-codecommit =
    if
      prev.awscli.version == "1.44.21" && prev.python3Packages.botocore.version != "1.42.31"
    then
      prev.git-remote-codecommit.overridePythonAttrs { doCheck = false; }
    else
      prev.git-remote-codecommit;
}
// prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin (
  prev.lib.optionalAttrs (v.firefox != null) {
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
  }
  // {
    # Without this, tcc on macos 27 stops firefox from reading its profile directory
    firefox = (final.wrapFirefox final.firefox-unwrapped { }).overrideAttrs (oldAttrs: {
      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ final.makeBinaryWrapper ];
    });
  }
  // prev.lib.optionalAttrs (v.google-chrome != null) {
    google-chrome = prev.google-chrome.overrideAttrs (
      finalAttrs: _: {
        inherit (v.google-chrome) version slug;
        src = prev.fetchurl {
          url = "http://dl.google.com/release2/chrome/${finalAttrs.slug}_${finalAttrs.version}/GoogleChrome-${finalAttrs.version}.dmg";
          inherit (v.google-chrome) hash;
        };
      }
    );
  }
  // prev.lib.optionalAttrs (v.orbstack != null) {
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
  }
  // {
    pear-desktop = prev.pear-desktop.overrideAttrs {
      dontStrip = true;
    };
  }
  // prev.lib.optionalAttrs (v.slack != null) {
    slack = prev.slack.overrideAttrs (
      finalAttrs: _: {
        inherit (v.slack) version;
        src = prev.fetchurl {
          url = "https://downloads.slack-edge.com/desktop-releases/mac/arm64/${finalAttrs.version}/Slack-${finalAttrs.version}-macOS.dmg";
          inherit (v.slack) hash;
        };
      }
    );
  }
  // prev.lib.optionalAttrs (v.zoom-us != null) {
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
)
