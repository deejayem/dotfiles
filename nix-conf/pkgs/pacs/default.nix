{
  lib,
  stdenv,
  fetchPnpmDeps,
  fetchPrivateNpm,
  runCommand,
  nodejs_24,
  pnpm_9,
  pnpmConfigHook,
  makeWrapper,
}:

let
  pname = "pacs-client";
  version = "1.3.0";

  baseSrc = fetchPrivateNpm {
    registry = "github";
    owner = "adzerk";
    name = pname;
    inherit version;
    downloadId = "1385d811c7890fc15572673135862a60417efd72";
    narHash = "sha256-gl7ITP6HHALSJLfQ7AmOn5jsYol2RzWBb/yOBB3LKzc=";
  };

  src = runCommand "${pname}-${version}-src" { } ''
    cp -R --no-preserve=mode,ownership ${baseSrc} $out
    cp ${./pnpm-lock.yaml} $out/pnpm-lock.yaml
  '';

  pnpmDeps = fetchPnpmDeps {
    inherit pname version src;
    pnpm = pnpm_9;
    fetcherVersion = 2;
    hash = "sha256-idV7LezKJBPNCkcJmGaBdK+90ochROtgZDF6UeSBiqw=";
  };
in

stdenv.mkDerivation {
  inherit pname version src;

  nativeBuildInputs = [
    nodejs_24
    pnpm_9
    pnpmConfigHook
    makeWrapper
  ];

  inherit pnpmDeps;

  HOME = "$TMPDIR";

  buildPhase = ''
    runHook preBuild

    pnpm install --frozen-lockfile --offline --prod --ignore-scripts

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib/${pname}" "$out/bin"
    cp -R dist package.json node_modules "$out/lib/${pname}/"

    makeWrapper ${lib.getExe nodejs_24} "$out/bin/pacs-aws" \
      --add-flags "$out/lib/${pname}/dist/aws/aws.js"
    makeWrapper ${lib.getExe nodejs_24} "$out/bin/pacs-gcp" \
      --add-flags "$out/lib/${pname}/dist/gcp/gcp.js"

    runHook postInstall
  '';

  meta = {
    description = "PACS client";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
