{ ... }:
final: prev: {
  cdktf-cli = prev.cdktf-cli.overrideAttrs (oldAttrs: {
    nativeBuildInputs = builtins.map (pkg:
      if pkg == prev.nodejs then prev.nodejs_22 else pkg
    ) oldAttrs.nativeBuildInputs;
    installPhase = builtins.replaceStrings
      [ "${prev.lib.getExe prev.nodejs}" ]
      [ "${prev.lib.getExe prev.nodejs_22}" ]
      oldAttrs.installPhase;
  });
}
