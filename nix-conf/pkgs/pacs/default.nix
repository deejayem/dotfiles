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
  version = "1.8.2";

  baseSrc = fetchPrivateNpm {
    registry = "github";
    owner = "adzerk";
    name = pname;
    inherit version;
    downloadId = "44fded14d2aa86fee538c6dd49cd57f1aaff2c2b";
    narHash = "sha256-etVy/WUnSk4UoYqjJLp1ogTST7R/Xj19j93tWte1ZK0=";
  };

  src = runCommand "${pname}-${version}-src" { } ''
    cp -R --no-preserve=mode,ownership ${baseSrc} $out
    cp ${./pnpm-lock.yaml} $out/pnpm-lock.yaml
  '';

  pnpmDeps = fetchPnpmDeps {
    inherit pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-8KLd2Z7oPlgCnCIljyjBZMpGXea/XjoFtYqsf/SfGgI=";
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
