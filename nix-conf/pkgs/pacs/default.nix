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
  version = "1.1.1";

  baseSrc = fetchPrivateNpm {
    registry = "github";
    owner = "adzerk";
    name = pname;
    inherit version;
    downloadId = "37bf89763e7cb87cc9daa946aaccef97dd75ce6b";
    narHash = "sha256-leWP4bccbCF7H+NwK54bYc/YsXtU2GkY40gpkX86I5c=";
  };

  src = runCommand "${pname}-${version}-src" { } ''
    cp -R --no-preserve=mode,ownership ${baseSrc} $out
    cp ${./pnpm-lock.yaml} $out/pnpm-lock.yaml
  '';

  pnpmDeps = fetchPnpmDeps {
    inherit pname version src;
    pnpm = pnpm_9;
    fetcherVersion = 2;
    hash = "sha256-wqJ8vtbt1ozRdKF8niigE+4Irm7JInsIONQDOcPOsk0=";
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
