{ ... }:
final: prev:
let
  goForTailscale = prev.go.overrideAttrs (
    _:
    let
      version = "1.25.5";
    in
    {
      inherit version;
      src = prev.fetchurl {
        url = "https://go.dev/dl/go${version}.src.tar.gz";
        hash = "sha256-IqX9CpHvzSihsFNxBrmVmygEth9Zw3WLUejlQpwalU8=";
      };
    }
  );
  buildGoModuleForTailscale = prev.buildGoModule.override {
    go = goForTailscale;
  };
in
{
  orbstack = prev.orbstack.overrideAttrs (
    _:
    let
      version = "2.0.5-19905";
    in
    {
      inherit version;

      src = prev.fetchurl {
        url = "https://cdn-updates.orbstack.dev/arm64/OrbStack_v${
          prev.lib.replaceString "-" "_" version
        }_arm64.dmg";
        hash = "sha256-YBiJVRzf3H/u4Ui3/bBID6C6XA2wvo8cBH/IQIhqdXE=";
      };
    }
  );
  tailscale = (prev.tailscale.override { buildGoModule = buildGoModuleForTailscale; }).overrideAttrs (
    _:
    let
      version = "1.92.3";
    in
    {
      inherit version;
      src = prev.fetchFromGitHub {
        owner = "tailscale";
        repo = "tailscale";
        tag = "v${version}";
        hash = "sha256-6dE3kgYABAVtrAjGWnWZ3X4Aq7yJagxNEk6BSyIC3Yk=";
      };
      vendorHash = "sha256-jJSSXMyUqcJoZuqfSlBsKDQezyqS+jDkRglMMjG1K8g=";
    }
  );
  zoom-us = prev.zoom-us.overrideAttrs (
    _:
    let
      system = prev.stdenv.hostPlatform.system;

      versions = {
        aarch64-darwin = "6.7.0.71075";
        x86_64-darwin = "6.7.0.71075";
        x86_64-linux = "6.7.0.6313";
      };

      srcs = {
        aarch64-darwin = prev.fetchurl {
          url = "https://zoom.us/client/${versions.aarch64-darwin}/zoomusInstallerFull.pkg?archType=arm64";
          name = "zoomusInstallerFull.pkg";
          hash = "sha256-Jj6Ikxk7W77sv6g6yYR9ttTRF3kooQgVJnExNaU5aAA=";
        };
        x86_64-darwin = prev.fetchurl {
          url = "https://zoom.us/client/${versions.x86_64-darwin}/zoomusInstallerFull.pkg";
          hash = "sha256-OaF3cbJZcXKKPkvdfjMuL1Va944N/TOAlqCLdA1fl64=";
        };
        x86_64-linux = prev.fetchurl {
          url = "https://zoom.us/client/${versions.x86_64-linux}/zoom_x86_64.pkg.tar.xz";
          hash = "sha256-Bm5LEGmGN0iUwzKdVzicxfx6K3g9FGevvR/gUIBaPj8=";
        };
      };
    in
    {
      version = versions.${system};
      src = srcs.${system};
    }
  );
}
