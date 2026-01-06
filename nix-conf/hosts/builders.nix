{ inputs, self }:
let
  nix-darwin = inputs.nix-darwin;
  inherit (inputs.nixpkgs.lib) optionals;

  mkNixosConfig =
    versions: hostname:
    {
      system,
      version,
      role,
      nixPlugins ? false,
      ...
    }:
    versions.withVersions version (
      { nixpkgs, ... }:
      nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs hostname role; };
        modules = [
          ../nix.nix
          ./nixos/${hostname}/configuration.nix
        ]
        ++ optionals nixPlugins [ ../nix-plugins ];
      }
    );

  mkDarwinConfig =
    versions: hostname:
    {
      role,
      nixPlugins ? false,
      ...
    }:
    nix-darwin.lib.darwinSystem {
      system.configurationRevision = self.rev or self.dirtyRev or null;
      specialArgs = { inherit inputs hostname role; };
      modules = [
        ../nix.nix
        ./darwin/${hostname}.nix
      ]
      ++ optionals nixPlugins [ ../nix-plugins ];
    };

  mkHomeConfig =
    versions: hostname: username:
    {
      system,
      version,
      role,
      nixPlugins ? false,
      ...
    }:
    versions.withVersions version (
      { nixpkgs, home-manager }:
      home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system};
        extraSpecialArgs = {
          inherit
            inputs
            hostname
            system
            version
            role
            ;
        };
        modules = [
          ../nix.nix
          ./home-manager/${hostname}.nix
        ]
        ++ optionals nixPlugins [ ../nix-plugins ];
      }
    );

  mapHosts =
    f: hosts: builtins.concatMap (hostname: f hostname hosts.${hostname}) (builtins.attrNames hosts);

  mkNixosConfigurations =
    versions: hosts:
    builtins.listToAttrs (
      mapHosts (
        hostname: cfg:
        if cfg ? nixos then
          [
            {
              name = hostname;
              value = (mkNixosConfig versions) hostname cfg;
            }
          ]
        else
          [ ]
      ) hosts
    );

  mkDarwinConfigurations =
    versions: hosts:
    builtins.listToAttrs (
      mapHosts (
        hostname: cfg:
        if cfg ? darwin then
          [
            {
              name = hostname;
              value = (mkDarwinConfig versions) hostname cfg;
            }
          ]
        else
          [ ]
      ) hosts
    );

  mkHomeConfigurations =
    versions: hosts:
    builtins.listToAttrs (
      mapHosts (
        hostname: cfg:
        if cfg ? home then
          builtins.map (username: {
            name = "${username}@${hostname}";
            value = (mkHomeConfig versions) hostname username cfg;
          }) (builtins.attrNames cfg.home)
        else
          [ ]
      ) hosts
    );
in
{
  mkConfigurations = versions: hosts: {
    nixosConfigurations = mkNixosConfigurations versions hosts;
    darwinConfigurations = mkDarwinConfigurations versions hosts;
    homeConfigurations = mkHomeConfigurations versions hosts;
  };
}
