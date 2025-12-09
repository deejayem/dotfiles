{ ... }:
final: prev: {
  orbstack = prev.orbstack.overrideAttrs (
    old:
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
}
