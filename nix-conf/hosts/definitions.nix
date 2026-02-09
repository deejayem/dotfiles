{ inputs }:
let
  roles = {
    workstation = "workstation";
    server = "server";
  };

  orgs = {
    kevel = "kevel";
  };

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

  systems = {
    x86_64-linux = "x86_64-linux";
    aarch64-linux = "aarch64-linux";
    aarch64-darwin = "aarch64-darwin";
  };
in
{
  inherit roles versions systems;

  hosts = {
    egalmoth = {
      system = systems.x86_64-linux;
      version = versions.stable;
      role = roles.workstation;
      nixos = { };
      home.djm = { };
    };

    edrahil = {
      system = systems.x86_64-linux;
      version = versions.stable;
      role = roles.server;
      nixos = { };
      home.djm = { };
    };

    djmuk1 = {
      system = systems.x86_64-linux;
      version = versions.stable;
      role = roles.server;
      nixos = { };
      home.djm = { };
    };

    djmuk2 = {
      system = systems.aarch64-linux;
      version = versions.stable;
      role = roles.server;
      nixos = { };
      home.djm = { };
    };

    grithnir = {
      system = systems.aarch64-darwin;
      version = versions.unstable;
      role = roles.workstation;
      org = orgs.kevel;
      darwin = { };
      home.djm = { };
    };
  };
}
