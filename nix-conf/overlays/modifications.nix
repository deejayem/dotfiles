{ ... }:
final: prev:
prev.lib.optionalAttrs prev.stdenv.isDarwin {
  gtk3 = prev.gtk3.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      (prev.fetchpatch {
        url = "https://raw.githubusercontent.com/NixOS/nixpkgs/4cd527d969eef3d54e8814582249f6aa1c3a0a6e/pkgs/development/libraries/gtk/patches/3.0-mr5531-backport.patch";
        hash = "sha256-vP0xmeKQazr93bTV+2kIwsNA+rZPmNd9iaUfpYOpD0M=";
      })
    ];
  });
}
