{ inputs, self, ... }:
{
  flake =
    let
      versions = rec {
        stable = "stable";
        unstable = "unstable";

        config = {
          ${stable} = {
            nixpkgs = inputs.nixpkgs-stable;
            home-manager = inputs.home-manager-stable;
          };
          ${unstable} = {
            nixpkgs = inputs.nixpkgs-unstable;
            home-manager = inputs.home-manager-unstable;
          };
        };

        get =
          v:
          config.${v}
            or (throw "Invalid version '${v}'. Must be one of: ${builtins.concatStringsSep ", " (builtins.attrNames config)}");

        withVersions = version: f: f (get version);
      };

      inherit (versions) stable unstable withVersions;

      sops-nix = inputs.sops-nix;
      nix-darwin = inputs.nix-darwin;
      nix-index-database = inputs.nix-index-database;

      systems = {
        x86_64-linux = "x86_64-linux";
        aarch64-linux = "aarch64-linux";
        aarch64-darwin = "aarch64-darwin";
      };

      hosts = {
        egalmoth = {
          system = systems.x86_64-linux;
          version = stable;
          nixos = { };
          home.djm = { };
        };

        edrahil = {
          system = systems.x86_64-linux;
          version = stable;
          nixos.extraModules = [ sops-nix.nixosModules.sops ];
          home.djm = { };
        };

        djmuk1 = {
          system = systems.x86_64-linux;
          version = stable;
          nixos = { };
          home.djm = { };
        };

        djmuk2 = {
          system = systems.aarch64-linux;
          version = stable;
          nixos = { };
          home.djm = { };
        };

        grithnir = {
          system = systems.aarch64-darwin;
          version = unstable;
          darwin = { };
          home.djm = { };
        };
      };

      mkNixosConfig =
        hostname:
        {
          system,
          version,
          nixos,
          ...
        }:
        withVersions version (
          { nixpkgs, ... }:
          nixpkgs.lib.nixosSystem {
            inherit system;
            specialArgs = { inherit inputs; };
            modules = [
              ../config.nix
              ./nixos/${hostname}/configuration.nix
            ]
            ++ (nixos.extraModules or [ ]);
          }
        );

      mkDarwinConfig =
        hostname:
        { darwin, ... }:
        nix-darwin.lib.darwinSystem {
          system.configurationRevision = self.rev or self.dirtyRev or null;
          specialArgs = { inherit inputs; };
          modules = [
            ../config.nix
            ./darwin/configuration.nix
          ]
          ++ (darwin.extraModules or [ ]);
        };

      mkHomeConfig =
        hostname: username:
        { system, version, ... }:
        withVersions version (
          { nixpkgs, home-manager }:
          home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.${system};
            extraSpecialArgs = {
              inherit inputs system version;
            };
            modules = [
              ../config.nix
              nix-index-database.homeModules.nix-index
              ./home-manager/${hostname}.nix
            ];
          }
        );

      mkConfigurations =
        hosts:
        let
          mapHosts =
            f: builtins.concatMap (hostname: f hostname hosts.${hostname}) (builtins.attrNames hosts);

          mkNixosConfigurations =
            hosts:
            builtins.listToAttrs (
              mapHosts (
                hostname: cfg:
                if cfg ? nixos then
                  [
                    {
                      name = hostname;
                      value = mkNixosConfig hostname cfg;
                    }
                  ]
                else
                  [ ]
              )
            );

          mkDarwinConfigurations =
            hosts:
            builtins.listToAttrs (
              mapHosts (
                hostname: cfg:
                if cfg ? darwin then
                  [
                    {
                      name = hostname;
                      value = mkDarwinConfig hostname cfg;
                    }
                  ]
                else
                  [ ]
              )
            );

          mkHomeConfigurations =
            hosts:
            builtins.listToAttrs (
              mapHosts (
                hostname: cfg:
                if cfg ? home then
                  builtins.map (username: {
                    name = "${username}@${hostname}";
                    value = mkHomeConfig hostname username cfg;
                  }) (builtins.attrNames cfg.home)
                else
                  [ ]
              )
            );
        in
        {
          nixosConfigurations = mkNixosConfigurations hosts;
          darwinConfigurations = mkDarwinConfigurations hosts;
          homeConfigurations = mkHomeConfigurations hosts;
        };
    in
    mkConfigurations hosts;
}
