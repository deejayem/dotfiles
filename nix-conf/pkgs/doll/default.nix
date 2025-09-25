{
  lib,
  stdenvNoCC,
  fetchurl,
  _7zz,
  nix-update-script,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "doll";
  version = "0.0.9.2";

  src = fetchurl {
    url = "https://github.com/xiaogdgenuine/Doll/releases/download/v${finalAttrs.version}/Doll.${finalAttrs.version}.dmg";
    hash = "sha256-+ctQuR/BI9A9ZMyeva+UB/6XZ5bNuaX/zYmk+dScTcU=";
  };

  dontBuild = true;
  dontFixup = true;

  nativeBuildInputs = [ _7zz ];

  sourceRoot = "Doll.app";

  installPhase = ''
    runHook preInstall

    mkdir -p $out/Applications/Doll.app
    cp -R . $out/Applications/Doll.app

    runHook postInstall
  '';


  meta = {
    description = "A mac app to help monitor your app badges";
    homepage = "https://github.com/xiaogdgenuine/Doll";
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    platforms = lib.platforms.darwin;
  };
})
