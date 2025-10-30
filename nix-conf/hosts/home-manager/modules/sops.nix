{
  config,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    inputs.sops-nix.homeManagerModules.sops
  ];

  sops = {
    age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
    defaultSopsFile = builtins.path {
      path = ./secrets.yaml;
      name = "home-secrets.yaml";
    };
  };

  home.packages = with pkgs; [
    sops
  ];
}
