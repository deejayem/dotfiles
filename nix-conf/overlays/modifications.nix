{ inputs, ... }:
final: prev: {
  cdktf-cli = prev.cdktf-cli.overrideAttrs (oldAttrs: {
    nativeBuildInputs = builtins.map (
      pkg: if pkg == prev.nodejs then prev.nodejs_22 else pkg
    ) oldAttrs.nativeBuildInputs;
    installPhase =
      builtins.replaceStrings [ "${prev.lib.getExe prev.nodejs}" ] [ "${prev.lib.getExe prev.nodejs_22}" ]
        oldAttrs.installPhase;
  });

  firebase-tools = prev.firebase-tools.override {
    buildNpmPackage = prev.buildNpmPackage.override {
      nodejs = prev.nodejs_22;
    };
  };

  ssm-session-manager-plugin = inputs.nixpkgs-ssm.legacyPackages.${final.stdenv.hostPlatform.system}.ssm-session-manager-plugin;
}
