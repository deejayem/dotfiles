{
  description = "NixOS, nix-darwin, and Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.05";
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
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-stable,
      nix-darwin,
      home-manager,
      home-manager-stable,
      sops-nix,
      ...
    }@inputs:
    let
      darwin-system = "aarch64-darwin";
      linux-system = "x86_64-linux";
      linux-arm-system = "aarch64-linux";
      darwin-pkgs = nixpkgs.legacyPackages.${darwin-system};
      linux-pkgs = nixpkgs-stable.legacyPackages.${linux-system};
      linux-arm-pkgs = nixpkgs-stable.legacyPackages.${linux-arm-system};
      darwin-overlay-unstable = final: prev: {
        unstable = nixpkgs.legacyPackages.${darwin-system};
      };
      linux-overlay-unstable = final: prev: {
        unstable = nixpkgs.legacyPackages.${linux-system};
      };
      linux-arm-overlay-unstable = final: prev: {
        unstable = nixpkgs.legacyPackages.${linux-arm-system};
      };
    in
    {
      nixosConfigurations."egalmoth" = nixpkgs-stable.lib.nixosSystem {
        system = linux-system;
        modules = [
          (
            { config, pkgs, ... }:
            {
              nixpkgs.overlays = [ linux-overlay-unstable ];
            }
          )
          ./config.nix
          ./machines/egalmoth/configuration.nix
        ];
      };
      nixosConfigurations."edrahil" = nixpkgs-stable.lib.nixosSystem {
        system = linux-system;
        modules = [
          (
            { config, pkgs, ... }:
            {
              nixpkgs.overlays = [ linux-overlay-unstable ];
            }
          )
          ./config.nix
          ./machines/edrahil/configuration.nix
          sops-nix.nixosModules.sops
        ];
      };
      nixosConfigurations."djmuk1" = nixpkgs-stable.lib.nixosSystem {
        system = linux-system;
        modules = [
          (
            { config, pkgs, ... }:
            {
              nixpkgs.overlays = [ linux-overlay-unstable ];
            }
          )
          ./config.nix
          ./machines/djmuk1/configuration.nix
        ];
      };
      nixosConfigurations."djmuk2" = nixpkgs-stable.lib.nixosSystem {
        system = linux-arm-system;
        modules = [
          (
            { config, pkgs, ... }:
            {
              nixpkgs.overlays = [ linux-arm-overlay-unstable ];
            }
          )
          ./config.nix
          ./machines/djmuk2/configuration.nix
        ];
      };

      darwinConfigurations."grithnir" = nix-darwin.lib.darwinSystem {
        system.configurationRevision = self.rev or self.dirtyRev or null;
        modules = [
          ./darwin/configuration.nix
          ./config.nix
        ];
      };
      homeConfigurations."djm@grithnir" = home-manager.lib.homeManagerConfiguration {
        pkgs = darwin-pkgs;
        extraSpecialArgs = {
          inherit inputs;
          system = darwin-system;
        };
        modules = [
          (
            { config, pkgs, ... }:
            {
              nixpkgs.overlays = [ darwin-overlay-unstable ];
            }
          )
          ./config.nix
          ./home/kevel.nix
        ];
      };
      homeConfigurations."djm@egalmoth" = home-manager-stable.lib.homeManagerConfiguration {
        pkgs = linux-pkgs;
        extraSpecialArgs = {
          inherit inputs;
          system = linux-system;
        };
        modules = [
          (
            { config, pkgs, ... }:
            {
              nixpkgs.overlays = [ linux-overlay-unstable ];
            }
          )
          ./config.nix
          ./home/egalmoth.nix
        ];
      };
      homeConfigurations."djm@edrahil" = home-manager-stable.lib.homeManagerConfiguration {
        pkgs = linux-pkgs;
        extraSpecialArgs = {
          inherit inputs;
          system = linux-system;
        };
        modules = [
          (
            { config, pkgs, ... }:
            {
              nixpkgs.overlays = [ linux-overlay-unstable ];
            }
          )
          ./config.nix
          ./home/edrahil.nix
        ];
      };
      homeConfigurations."djm@djmuk1" = home-manager-stable.lib.homeManagerConfiguration {
        pkgs = linux-pkgs;
        extraSpecialArgs = {
          inherit inputs;
          system = linux-system;
        };
        modules = [
          (
            { config, pkgs, ... }:
            {
              nixpkgs.overlays = [ linux-overlay-unstable ];
            }
          )
          ./config.nix
          ./home/djmuk1.nix
        ];
      };
      homeConfigurations."djm@djmuk2" = home-manager-stable.lib.homeManagerConfiguration {
        pkgs = linux-arm-pkgs;
        extraSpecialArgs = {
          inherit inputs;
          system = linux-arm-system;
        };
        modules = [
          (
            { config, pkgs, ... }:
            {
              nixpkgs.overlays = [ linux-arm-overlay-unstable ];
            }
          )
          ./config.nix
          ./home/djmuk2.nix
        ];
      };
    };
}
