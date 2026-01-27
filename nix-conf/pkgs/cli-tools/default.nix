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
    rev = "8689c89122c9a382d5f1d78020c82aa36ccc4944";
    narHash = "sha256-py3I6hkhF7Bm/TOuCLAI9k1/mV9MPerNB2ENmQDNUpQ=";
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
