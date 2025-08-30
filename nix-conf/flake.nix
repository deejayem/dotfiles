{
  description = "NixOS, nix-darwin, and Home Manager configuration";

  inputs = {
    nixpkgs-stable.url = "nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };
    home-manager-unstable = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs =
    {
      self,
      nixpkgs-stable,
      nixpkgs-unstable,
      home-manager-stable,
      home-manager-unstable,
      sops-nix,
      nix-darwin,
      nix-index-database,
      ...
    }@inputs:
    let
      inherit (self) outputs;

      mkSystem = { system, nixpkgs, modules }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs outputs; };
          modules = [ ./config.nix ] ++ modules;
        };

      mkNixosConfig = { hostname, system, nixpkgs, extraModules ? [] }:
        mkSystem {
          inherit system nixpkgs;
          modules = [ ./machines/${hostname}/configuration.nix ] ++ extraModules;
        };

      mkHome = { system, home-manager, nixpkgs, modules }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          extraSpecialArgs = {
            inherit inputs outputs system;
            systemType = if nixpkgs == nixpkgs-unstable then "unstable" else "stable";
          };
          modules = [ ./config.nix nix-index-database.homeModules.nix-index ] ++ modules;
        };

      mkHomeConfig = { hostname, system, home-manager, nixpkgs, extraModules ? [] }:
        mkHome {
          inherit system home-manager nixpkgs;
          modules = [ ./home/${hostname}.nix ] ++ extraModules;
        };

      nixosHosts = {
        egalmoth = {
          system = "x86_64-linux";
          nixpkgs = nixpkgs-stable;
        };
        edrahil = {
          system = "x86_64-linux";
          nixpkgs = nixpkgs-stable;
          extraModules = [ sops-nix.nixosModules.sops ];
        };
        djmuk1 = {
          system = "x86_64-linux";
          nixpkgs = nixpkgs-stable;
        };
        djmuk2 = {
          system = "aarch64-linux";
          nixpkgs = nixpkgs-stable;
        };
      };

      homeHosts = {
        "djm@egalmoth" = {
          system = "x86_64-linux";
          nixpkgs = nixpkgs-stable;
          home-manager = home-manager-stable;
        };
        "djm@edrahil" = {
          system = "x86_64-linux";
          nixpkgs = nixpkgs-stable;
          home-manager = home-manager-stable;
        };
        "djm@djmuk1" = {
          system = "x86_64-linux";
          nixpkgs = nixpkgs-stable;
          home-manager = home-manager-stable;
        };
        "djm@djmuk2" = {
          system = "aarch64-linux";
          nixpkgs = nixpkgs-stable;
          home-manager = home-manager-stable;
        };
        "djm@grithnir" = {
          system = "aarch64-darwin";
          nixpkgs = nixpkgs-unstable;
          home-manager = home-manager-unstable;
        };
      };
    in
    {
      overlays = import ./overlays { inherit inputs; };

      nixosConfigurations =
        builtins.mapAttrs
          (hostname: cfg: mkNixosConfig ({ inherit hostname; } // cfg))
          nixosHosts;

      homeConfigurations =
        builtins.mapAttrs
          (username: cfg:
            let
              hostname = builtins.elemAt (builtins.match "([^@]+)@([^@]+)" username) 1;
            in
              mkHomeConfig ({ inherit hostname; } // cfg)
          )
          homeHosts;

      darwinConfigurations."grithnir" = nix-darwin.lib.darwinSystem {
        system.configurationRevision = self.rev or self.dirtyRev or null;
        specialArgs = { inherit inputs outputs; };
        modules = [
          ./darwin/configuration.nix
          ./config.nix
        ];
      };

    };
}
