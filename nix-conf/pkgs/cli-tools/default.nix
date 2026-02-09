{
  lib,
  stdenvNoCC,
  fetchFromPrivateGitHub,
}:

stdenvNoCC.mkDerivation {
  pname = "cli-tools";
  version = "0-unstable-2026-01-23";

  src = fetchFromPrivateGitHub {
    owner = "adzerk";
    repo = "cli-tools";
    rev = "29ec2846b5ad3a42db06cfd8ebd437b5602e68c2";
    narHash = "sha256-CrkBRTors5d8l8AVSlwAfkqcmKDIdyhS60w+c+Twvbc=";
  };

  installPhase = ''
    runHook preInstall

    find micha -maxdepth 1 -type f -exec install -Dm755 -t $out/bin {} +

    runHook postInstall
  '';

  meta = {
    description = "CLI tools from adzerk/cli-tools/micha";
    platforms = lib.platforms.unix;
  };
}
