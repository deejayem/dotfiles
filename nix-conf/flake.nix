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
    mcp-nixos = {
      url = "github:utensils/mcp-nixos";
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

      systems = {
        x86_64-linux = "x86_64-linux";
        aarch64-linux = "aarch64-linux";
        aarch64-darwin = "aarch64-darwin";
      };

      versions = {
        stable = "stable";
        unstable = "unstable";
      };

      versionConfig = {
        ${versions.stable} = {
          nixpkgs = nixpkgs-stable;
          home-manager = home-manager-stable;
        };
        ${versions.unstable} = {
          nixpkgs = nixpkgs-unstable;
          home-manager = home-manager-unstable;
        };
      };

      getVersions =
        v:
        versionConfig.${v}
          or (throw "Invalid version '${v}'. Must be one of: ${builtins.concatStringsSep ", " (builtins.attrNames versionConfig)}");

      withVersions = version: f: f (getVersions version);

      extractHostname = userAtHost: builtins.elemAt (builtins.split "@" userAtHost) 2;

      mkNixosConfig =
        {
          hostname,
          system,
          version,
          extraModules ? [ ],
        }:
        withVersions version (
          { nixpkgs, ... }:
          nixpkgs.lib.nixosSystem {
            inherit system;
            specialArgs = { inherit inputs outputs; };
            modules = [
              ./config.nix
              ./machines/${hostname}/configuration.nix
            ]
            ++ extraModules;
          }
        );

      mkHomeConfig =
        {
          hostname,
          system,
          version,
          extraModules ? [ ],
        }:
        withVersions version (
          { nixpkgs, home-manager }:
          home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.${system};
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
          }
        );

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
          system = systems.x86_64-linux;
          version = versions.stable;
        };
        edrahil = {
          system = systems.x86_64-linux;
          version = versions.stable;
          extraModules = [ sops-nix.nixosModules.sops ];
        };
        djmuk1 = {
          system = systems.x86_64-linux;
          version = versions.stable;
        };
        djmuk2 = {
          system = systems.aarch64-linux;
          version = versions.stable;
        };
      };

      homeHosts = {
        "djm@egalmoth" = {
          system = systems.x86_64-linux;
          version = versions.stable;
        };
        "djm@edrahil" = {
          system = systems.x86_64-linux;
          version = versions.stable;
        };
        "djm@djmuk1" = {
          system = systems.x86_64-linux;
          version = versions.stable;
        };
        "djm@djmuk2" = {
          system = systems.aarch64-linux;
          version = versions.stable;
        };
        "djm@grithnir" = {
          system = systems.aarch64-darwin;
          version = versions.unstable;
        };
      };

      darwinHosts = {
        grithnir = {
          system = systems.aarch64-darwin;
        };
      };

      mkFormatters =
        systems:
        let
          nixpkgsFor = system: nixpkgs-unstable.legacyPackages.${system};
        in
        builtins.listToAttrs (
          map (system: {
            name = system;
            value = (nixpkgsFor system).nixfmt-tree;
          }) systems
        );

    in
    {
      formatter = mkFormatters (builtins.attrValues systems);

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
