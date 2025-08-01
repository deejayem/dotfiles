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
      nixpkgs-config = {
        allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [ "aspell-dict-en-science" ];
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
              #system.primaryUser = "dmorgan"; # required to update com.apple.symbolichotkeys
              system.keyboard.enableKeyMapping = true;
              system.keyboard.userKeyMapping = [
                { HIDKeyboardModifierMappingSrc = 30064771296; HIDKeyboardModifierMappingDst = 30064771299; }
                { HIDKeyboardModifierMappingSrc = 30064771299; HIDKeyboardModifierMappingDst = 30064771296; }
              ];
              #system.defaults.CustomUserPreferences = {
              #  "com.apple.symbolichotkeys" = {
              #    AppleSymbolicHotKeys = {
              #      "60" = {
              #        enabled = 0;
              #      };
              #      "61" = {
              #        enabled = 0;
              #      };
              #    };
              #  };
              #};
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
          # TODO remove or re-enable?
          #home-manager.darwinModules.home-manager
          #{
          #  nixpkgs.overlays = [ darwin-overlay-unstable ];
          #  nixpkgs.config = nixpkgs-config;
          #  home-manager = {
          #    useGlobalPkgs = true;
          #    useUserPackages = true;
          #    extraSpecialArgs = {
          #      inherit inputs;
          #      system = darwin-system;
          #    };
          #    users.dmorgan = ./home/otm.nix;
          #  };
          #}
        ];
      };
      homeConfigurations."dmorgan@LDN-DMORGAN" = home-manager.lib.homeManagerConfiguration {
        pkgs = darwin-pkgs;
        extraSpecialArgs = { inherit inputs; system = darwin-system; };
        modules = [
          ({ config, pkgs, ...  }: { nixpkgs.overlays = [ darwin-overlay-unstable ]; nixpkgs.config = nixpkgs-config; nix.package = pkgs.nix; })
          ./home/otm.nix
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
              nix.package = pkgs.nix;
              nixpkgs.overlays = [ linux-overlay-unstable ];
              nixpkgs.config = nixpkgs-config;
            }
          )
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
              nix.package = pkgs.nix;
              nixpkgs.overlays = [ linux-overlay-unstable ];
              nixpkgs.config = nixpkgs-config;
            }
          )
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
              nix.package = pkgs.nix;
              nixpkgs.overlays = [ linux-overlay-unstable ];
              nixpkgs.config = nixpkgs-config;
            }
          )
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
              nix.package = pkgs.nix;
              nixpkgs.overlays = [ linux-arm-overlay-unstable ];
              nixpkgs.config = nixpkgs-config;
            }
          )
          ./home/djmuk2.nix
        ];
      };
    };
}
