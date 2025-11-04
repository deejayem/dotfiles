{ inputs, self }:
let
  nix-darwin = inputs.nix-darwin;

  mkNixosConfig =
    versions: hostname:
    {
      system,
      version,
      role,
      ...
    }:
    versions.withVersions version (
      { nixpkgs, ... }:
      nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs role; };
        modules = [
          ../nix.nix
          ./nixos/${hostname}/configuration.nix
        ];
      }
    );

  mkDarwinConfig =
    versions: hostname:
    {
      role,
      ...
    }:
    nix-darwin.lib.darwinSystem {
      system.configurationRevision = self.rev or self.dirtyRev or null;
      specialArgs = { inherit inputs role; };
      modules = [
        ../nix.nix
        ./darwin/${hostname}.nix
      ];
    };

  mkHomeConfig =
    versions: hostname: username:
    {
      system,
      version,
      role,
      ...
    }:
    versions.withVersions version (
      { nixpkgs, home-manager }:
      home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system};
        extraSpecialArgs = {
          inherit
            inputs
            system
            version
            role
            ;
        };
        modules = [
          ../nix.nix
          ./home-manager/${hostname}.nix
        ];
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
