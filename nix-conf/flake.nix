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

      resolveVersions =
        version:
        {
          stable = {
            nixpkgs = nixpkgs-stable;
            home-manager = home-manager-stable;
          };
          unstable = {
            nixpkgs = nixpkgs-unstable;
            home-manager = home-manager-unstable;
          };
        }
        .${version};

      extractHostname = userAtHost: builtins.elemAt (builtins.split "@" userAtHost) 2;

      mkNixosConfig =
        {
          hostname,
          system,
          version,
          extraModules ? [ ],
        }:
        let
          versions = resolveVersions version;
        in
        versions.nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs outputs; };
          modules = [
            ./config.nix
            ./machines/${hostname}/configuration.nix
          ]
          ++ extraModules;
        };

      mkHomeConfig =
        {
          hostname,
          system,
          version,
          extraModules ? [ ],
        }:
        let
          versions = resolveVersions version;
        in
        versions.home-manager.lib.homeManagerConfiguration {
          pkgs = versions.nixpkgs.legacyPackages.${system};
          extraSpecialArgs = {
            inherit
              inputs
              outputs
              system
              version
              ;
          };
          modules = [
            ./config.nix
            nix-index-database.homeModules.nix-index
            ./home/${hostname}.nix
          ]
          ++ extraModules;
        };

      mkDarwinConfig =
        {
          hostname,
          system,
          extraModules ? [ ],
        }:
        nix-darwin.lib.darwinSystem {
          system.configurationRevision = self.rev or self.dirtyRev or null;
          specialArgs = { inherit inputs outputs; };
          modules = [
            ./config.nix
            ./darwin/configuration.nix
          ]
          ++ extraModules;
        };

      nixosHosts = {
        egalmoth = {
          system = "x86_64-linux";
          version = "stable";
        };
        edrahil = {
          system = "x86_64-linux";
          version = "stable";
          extraModules = [ sops-nix.nixosModules.sops ];
        };
        djmuk1 = {
          system = "x86_64-linux";
          version = "stable";
        };
        djmuk2 = {
          system = "aarch64-linux";
          version = "stable";
        };
      };

      homeHosts = {
        "djm@egalmoth" = {
          system = "x86_64-linux";
          version = "stable";
        };
        "djm@edrahil" = {
          system = "x86_64-linux";
          version = "stable";
        };
        "djm@djmuk1" = {
          system = "x86_64-linux";
          version = "stable";
        };
        "djm@djmuk2" = {
          system = "aarch64-linux";
          version = "stable";
        };
        "djm@grithnir" = {
          system = "aarch64-darwin";
          version = "unstable";
        };
      };

      darwinHosts = {
        grithnir = {
          system = "aarch64-darwin";
        };
      };
    in
    {
      overlays = import ./overlays { inherit inputs; };

      nixosConfigurations = builtins.mapAttrs (
        hostname: cfg: mkNixosConfig ({ inherit hostname; } // cfg)
      ) nixosHosts;

      homeConfigurations = builtins.mapAttrs (
        username: cfg:
        let
          hostname = extractHostname username;
        in
        mkHomeConfig ({ inherit hostname; } // cfg)
      ) homeHosts;

      darwinConfigurations = builtins.mapAttrs (
        hostname: cfg: mkDarwinConfig ({ inherit hostname; } // cfg)
      ) darwinHosts;
    };
}
