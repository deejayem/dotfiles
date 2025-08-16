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
      inherit (self) outputs;

      mkSystem =
        {
          system,
          nixpkgs,
          modules,
        }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs outputs; };
          modules = [ ./config.nix ] ++ modules;
        };

      mkHome =
        {
          system,
          home-manager,
          nixpkgs,
          modules,
        }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."${system}";
          extraSpecialArgs = {
            inherit inputs outputs system;
          };
          modules = [ ./config.nix ] ++ modules;
        };
    in
    {
      overlays = import ./overlays { inherit inputs; };

      nixosConfigurations."egalmoth" = mkSystem {
        system = "x86_64-linux";
        nixpkgs = nixpkgs-stable;
        modules = [
          ./machines/egalmoth/configuration.nix
        ];
      };

      nixosConfigurations."edrahil" = mkSystem {
        system = "x86_64-linux";
        nixpkgs = nixpkgs-stable;
        modules = [
          ./machines/edrahil/configuration.nix
          sops-nix.nixosModules.sops
        ];
      };

      nixosConfigurations."djmuk1" = mkSystem {
        system = "x86_64-linux";
        nixpkgs = nixpkgs-stable;
        modules = [
          ./machines/djmuk1/configuration.nix
        ];
      };

      nixosConfigurations."djmuk2" = mkSystem {
        system = "aarch64-linux";
        nixpkgs = nixpkgs-stable;
        modules = [
          ./machines/djmuk2/configuration.nix
        ];
      };

      darwinConfigurations."grithnir" = nix-darwin.lib.darwinSystem {
        system.configurationRevision = self.rev or self.dirtyRev or null;
        specialArgs = { inherit inputs outputs; };
        modules = [
          ./darwin/configuration.nix
          ./config.nix
        ];
      };

      homeConfigurations."djm@egalmoth" = mkHome {
        system = "x86_64-linux";
        home-manager = home-manager-stable;
        nixpkgs = nixpkgs-stable;
        modules = [ ./home/egalmoth.nix ];
      };

      homeConfigurations."djm@edrahil" = mkHome {
        system = "x86_64-linux";
        home-manager = home-manager-stable;
        nixpkgs = nixpkgs-stable;
        modules = [ ./home/edrahil.nix ];
      };

      homeConfigurations."djm@djmuk1" = mkHome {
        system = "x86_64-linux";
        home-manager = home-manager-stable;
        nixpkgs = nixpkgs-stable;
        modules = [ ./home/djmuk1.nix ];
      };

      homeConfigurations."djm@djmuk2" = mkHome {
        system = "aarch64-linux";
        home-manager = home-manager-stable;
        nixpkgs = nixpkgs-stable;
        modules = [ ./home/djmuk2.nix ];
      };

      homeConfigurations."djm@grithnir" = mkHome {
        system = "aarch64-darwin";
        inherit home-manager nixpkgs;
        modules = [ ./home/kevel.nix ];
      };

    };
}
