{ ... }:
final: prev: {
  cdktf-cli = prev.cdktf-cli.overrideAttrs (oldAttrs: {
    nativeBuildInputs = builtins.map (pkg:
      if pkg == prev.nodejs then prev.nodejs_22 else pkg
    ) oldAttrs.nativeBuildInputs;
    installPhase = builtins.replaceStrings
      [ "${prev.lib.getExe prev.nodejs}" ]
      [ "${prev.lib.getExe prev.nodejs_22}" ]
      oldAttrs.installPhase;
  });
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
  zoom-us = prev.zoom-us.overrideAttrs (
    _:
    let
      system = prev.stdenv.hostPlatform.system;

      versions = {
        aarch64-darwin = "6.7.2.72191";
        x86_64-darwin = "6.7.2.72191";
        x86_64-linux = "6.7.2.6498";
      };

      srcs = {
        aarch64-darwin = prev.fetchurl {
          url = "https://zoom.us/client/${versions.aarch64-darwin}/zoomusInstallerFull.pkg?archType=arm64";
          name = "zoomusInstallerFull.pkg";
          hash = "sha256-v4maXehUE0KSmZO2OI/d2qVyeTnGvkaWaQzul/yde80=";
        };
        x86_64-darwin = prev.fetchurl {
          url = "https://zoom.us/client/${versions.x86_64-darwin}/zoomusInstallerFull.pkg";
          hash = "sha256-6hDDzxzZMisPPyrIQ6XIhipVbid4a/X2gaV4DYIfz3w=";
        };
        x86_64-linux = prev.fetchurl {
          url = "https://zoom.us/client/${versions.x86_64-linux}/zoom_x86_64.pkg.tar.xz";
          hash = "sha256-fvZMKjzQFkIKl3TUOMYHR75SDHShkKjk59ps55o9Qks=";
        };
      };
    in
    {
      version = versions.${system};
      src = srcs.${system};
    }
  );
}
