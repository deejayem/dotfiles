{
  lib,
  stdenv,
  fetchPnpmDeps,
  fetchPrivateNpm,
  runCommand,
  nodejs_24,
  pnpm_10,
  pnpmConfigHook,
  makeWrapper,
}:

let
  pname = "pacs-client";
  version = "1.7.0";

  baseSrc = fetchPrivateNpm {
    registry = "github";
    owner = "adzerk";
    name = pname;
    inherit version;
    downloadId = "1c54999bcfe9970bbee73b48e27a05ea4c03a542";
    narHash = "sha256-L1Q8u6N/5/scvEaiREqxctzL3DeBpH0KFlSb9dtfFzU=";
  };

  src = runCommand "${pname}-${version}-src" { } ''
    cp -R --no-preserve=mode,ownership ${baseSrc} $out
    cp ${./pnpm-lock.yaml} $out/pnpm-lock.yaml
  '';

  pnpmDeps = fetchPnpmDeps {
    inherit pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-ZD3EldX2VIr79yLk4MkNf92pOLprrC2RQZWjNS5O6GA=";
  };
in

stdenv.mkDerivation {
  inherit pname version src;

  nativeBuildInputs = [
    nodejs_24
    pnpm_10
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
