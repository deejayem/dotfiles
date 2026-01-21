{
  lib,
  stdenv,
  fetchPnpmDeps,
  runCommand,
  nodejs_24,
  pnpm_9,
  pnpmConfigHook,
  makeWrapper,
}:

let
  pname = "pacs-client";
  version = "1.0.2";
  downloadId = "ed132c2ef5f061c27d3f5dbaea3229cefde4a892";

  # Use builtins.fetchTree as it supports netrc (nix.settings.netrc-file)
  baseSrc = builtins.fetchTree {
    type = "tarball";
    url = "https://npm.pkg.github.com/download/@adzerk/pacs-client/${version}/${downloadId}";
    narHash = "sha256-wm/9zH5J4u2BfTzJ5OupdLhV0CdLqr9bPLiNNnzmWnA=";
  };

  src = runCommand "${pname}-${version}-src" { } ''
    cp -R --no-preserve=mode,ownership ${baseSrc} $out
    cp ${./pnpm-lock.yaml} $out/pnpm-lock.yaml
  '';

  pnpmDeps = fetchPnpmDeps {
    inherit pname version src;
    pnpm = pnpm_9;
    fetcherVersion = 2;
    hash = "sha256-dplFyycPCO2aJg29Ic74fiHbvo61W/dtZNcdX15pLwk=";
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

  meta = with lib; {
    description = "PACS client";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
