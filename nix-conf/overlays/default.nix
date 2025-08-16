{ inputs, ... }:
{
  unstable-packages = final: _prev: {
    # inputs.nixpkgs uses the unstable branch
    unstable = import inputs.nixpkgs {
      system = final.system;
    };
  };
}
