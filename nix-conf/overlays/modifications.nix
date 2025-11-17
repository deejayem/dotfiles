{ ... }:
final: prev: {
  fish = prev.fish.overrideAttrs (oldAttrs: {
    sandboxProfile = "";
    doCheck = false;
  });
}
