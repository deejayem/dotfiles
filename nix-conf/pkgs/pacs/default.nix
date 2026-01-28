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
  version = "1.1.0";

  baseSrc = fetchPrivateNpm {
    registry = "github";
    owner = "adzerk";
    name = pname;
    inherit version;
    downloadId = "9a3b64d705dd0579b6b3e9eb6a686bf786546f70";
    narHash = "sha256-71xF4CcnAPMBBZZKPoYTAJzQu9/YOJAshkS8XIJP3RE=";
  };

  src = runCommand "${pname}-${version}-src" { } ''
    cp -R --no-preserve=mode,ownership ${baseSrc} $out
    cp ${./pnpm-lock.yaml} $out/pnpm-lock.yaml
  '';

  pnpmDeps = fetchPnpmDeps {
    inherit pname version src;
    pnpm = pnpm_9;
    fetcherVersion = 2;
    hash = "sha256-m+5g7mcVRqYYClgR4vPkEi8m4ygGipVCFH2A1zgzxJc=";
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
