{
  lib,
  stdenv,
  fetchurl,
  fetchPnpmDeps,
  runCommand,
  nodejs_22,
  pnpm_9,
  pnpmConfigHook,
  makeWrapper,
  npmToken ? throw "npmToken is missing",
}:

let
  pname = "pacs-client";
  version = "0.0.12";

  tgz = fetchurl {
    name = "${pname}-${version}.tgz";
    url = "https://npm.pkg.github.com/download/@adzerk/pacs-client/${version}/6281fea7fdbf81bb8e6737cbf3e217888b94bab1";
    hash = "sha256-Howkc055M7CTAT8wH6L3JDUHbFHkzSosAIGtN1OQtgM=";
    curlOptsList = [
      "-H"
      "Authorization: Bearer ${npmToken}"
    ];
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
    nodejs_22
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

    makeWrapper ${lib.getExe nodejs_22} "$out/bin/pacs" \
      --add-flags "$out/lib/${pname}/dist/index.js"

    runHook postInstall
  '';

  meta = with lib; {
    description = "PACS client";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
