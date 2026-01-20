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

  # Use builtins.fetchurl as it supports netrc (nix.settings.netrc-file)
  tgz = builtins.fetchurl {
    name = "${pname}-${version}.tgz";
    url = "https://npm.pkg.github.com/download/@adzerk/pacs-client/${version}/ed132c2ef5f061c27d3f5dbaea3229cefde4a892";
    sha256 = "1lncbyi3564snv149if0yqx944qcawwa0kyncjh643sj848lj2dp";
  };

  src = runCommand "${pname}-${version}-src" { } ''
    mkdir -p $out
    tar -xzf ${tgz} -C $out --strip-components=1
    cp ${./pnpm-lock.yaml} $out/pnpm-lock.yaml
  '';

  pnpmDeps = fetchPnpmDeps {
    inherit pname version src;
    pnpm = pnpm_9;
    fetcherVersion = 2;
    hash = "sha256-vh4MiZvtIyGJOWKuycDH044pdQiYfOGEhrApiSFrLyc=";
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
