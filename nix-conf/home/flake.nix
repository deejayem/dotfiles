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
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin-system-certs = {
      url = "/private/etc/ssl/cert.pem";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-stable,
      nixpkgs-unstable,
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
      darwinConfigurations."LDN-DMORGAN" = nix-darwin.lib.darwinSystem {
        modules = [
          # TODO move to separate file
          (
            { pkgs, ... }:
            {
              nix.settings.experimental-features = "nix-command flakes";
              nix.settings.trusted-users = [
                "dmorgan"
                "@staff"
              ];
              nix.settings.ssl-cert-file = "/Users/dmorgan/certs/full-cert.pem";
              system.configurationRevision = self.rev or self.dirtyRev or null;
              system.stateVersion = 6;
              nixpkgs.hostPlatform = "aarch64-darwin";
              ids.gids.nixbld = 30000;
              users.users.dmorgan.home = "/Users/dmorgan";
              fonts.packages = [
                pkgs.aporetic
                pkgs.meslo-lgs-nf
                pkgs.fira-code
              ];
            }
          )
          home-manager.darwinModules.home-manager
          {
            nixpkgs.overlays = [ darwin-overlay-unstable ];
            nixpkgs.config.allowUnfreePredicate =
              pkg: builtins.elem (nixpkgs.lib.getName pkg) [ "aspell-dict-en-science" ];
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = {
                inherit inputs;
                system = darwin-system;
              };
              users.dmorgan = ./otm.nix;
            };
          }
        ];
      };
      # WIP: TODO: migrate home configs to nixos config
      # nh home switch -a ~/dotfiles/nix-conf/home -c $(whoami)-$(hostname)
      homeConfigurations."djm-egalmoth" = home-manager-stable.lib.homeManagerConfiguration {
        pkgs = linux-pkgs;
        extraSpecialArgs = {
          inherit inputs;
          system = linux-system;
        };
        modules = [
          (
            { config, pkgs, ... }:
            {
              nix.package = pkgs.nix;
              nixpkgs.overlays = [ linux-overlay-unstable ];
              nixpkgs.config.allowUnfreePredicate =
                pkg: builtins.elem (nixpkgs.lib.getName pkg) [ "aspell-dict-en-science" ];
            }
          )
          ./egalmoth.nix
        ];
      };
    };
}
