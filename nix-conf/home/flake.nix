{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin-system-certs = {
      url = "/private/etc/ssl/cert.pem";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, nixpkgs-stable, nixpkgs-unstable, home-manager, home-manager-stable, sops-nix, ... }@inputs:
    let
      darwin-system = "aarch64-darwin";
      linux-system = "x86_64-linux";
      linux-arm-system = "aarch64-linux";
      darwin-pkgs = nixpkgs.legacyPackages.${darwin-system};
      linux-pkgs = nixpkgs-stable.legacyPackages.${linux-system};
      darwin-overlay-unstable = final: prev: {
        unstable = nixpkgs-unstable.legacyPackages.${darwin-system};
      };
      linux-overlay-unstable = final: prev: {
        unstable = nixpkgs-unstable.legacyPackages.${linux-system};
      };
      linux-arm-overlay-unstable = final: prev: {
        unstable = nixpkgs-unstable.legacyPackages.${linux-arm-system};
      };
    in
    {
      homeConfigurations."dmorgan" = home-manager.lib.homeManagerConfiguration {
        pkgs = darwin-pkgs;
        extraSpecialArgs = { inherit inputs; system = darwin-system; };
        modules = [
          ({ config, pkgs, ...  }: { nixpkgs.overlays = [ darwin-overlay-unstable ]; })
          ./otm.nix
        ];
      };
      homeConfigurations."djm-egalmoth" = home-manager-stable.lib.homeManagerConfiguration {
        pkgs = linux-pkgs;
        extraSpecialArgs = { inherit inputs; system = linux-system; };
        modules = [
          ({ config, pkgs, ...  }: { nixpkgs.overlays = [ linux-overlay-unstable ]; })
          ./egalmoth.nix
        ];
      };
    };
}
