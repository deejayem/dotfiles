{ inputs, ... }:
final: _prev: {
  unstable = import inputs.nixpkgs-unstable {
    system = final.pkgs.stdenv.hostPlatform.system;
  };
}
