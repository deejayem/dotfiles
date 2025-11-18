{ ... }:
final: prev:
prev.lib.optionalAttrs prev.stdenv.isDarwin {
  fish = prev.fish.overrideAttrs (oldAttrs: {
    sandboxProfile = "";
    doCheck = false;
  });
}
