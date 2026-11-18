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
    if prev.awscli.version == "1.44.21" && prev.python3Packages.botocore.version != "1.42.31" then
      prev.git-remote-codecommit.overridePythonAttrs { doCheck = false; }
    else
      prev.git-remote-codecommit;
}
// prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin (
  prev.lib.optionalAttrs (v.brave != null) {
    brave = prev.brave.overrideAttrs (
      finalAttrs: _: {
        inherit (v.brave) version;
        src = prev.fetchurl {
          url = "https://github.com/brave/brave-browser/releases/download/v${finalAttrs.version}/Brave-Browser-arm64.dmg";
          inherit (v.brave) hash;
        };
        sourceRoot = "Brave Browser.app";
      }
    );
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
  // prev.lib.optionalAttrs (v.librewolf != null) {
    # nixpkgs reads the sources from src.json via a nested callPackage, so
    # swap out what librewolf.nix gets when it calls ./src.nix.
    librewolf-unwrapped =
      let
        src = {
          packageVersion = v.librewolf.version;
          source = final.fetchFromForgejo {
            domain = "librewolf.dev";
            owner = "librewolf";
            repo = "source";
            rev = v.librewolf.version;
            inherit (v.librewolf) hash;
          };
          firefox = final.fetchurl {
            version = v.librewolf.firefoxVersion;
            url = "mirror://mozilla/firefox/releases/${v.librewolf.firefoxVersion}/source/firefox-${v.librewolf.firefoxVersion}.source.tar.xz";
            sha512 = v.librewolf.firefoxSha512;
          };
        };
      in
      final.callPackage (prev.path + "/pkgs/by-name/li/librewolf-unwrapped/package.nix") {
        callPackage =
          fn: args:
          if baseNameOf (toString fn) == "librewolf.nix" then
            final.callPackage fn (args // { callPackage = _: _: src; })
          else
            final.callPackage fn args;
      };
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
