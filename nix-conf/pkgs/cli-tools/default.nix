{
  lib,
  pkgs,
  stdenvNoCC,
  fetchFromPrivateGitHub,
  makeWrapper,
}:

stdenvNoCC.mkDerivation {
  pname = "cli-tools";
  version = "0-unstable-2026-02-03";

  src = fetchFromPrivateGitHub {
    owner = "adzerk";
    repo = "cli-tools";
    rev = "29ec2846b5ad3a42db06cfd8ebd437b5602e68c2";
    narHash = "sha256-CrkBRTors5d8l8AVSlwAfkqcmKDIdyhS60w+c+Twvbc=";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    find micha -maxdepth 1 -type f -exec install -Dm755 -t "$out/bin" {} +
    runtimePath="${
      lib.makeBinPath (
        with pkgs;
        [
          envsubst
          awscli2
          curl
          json-table
          jq
          parallel
          redis
          jo
          openssl
          pv
        ]
      )
    }"

    # Inject $PATH, rather than using wrapProgram, to avoid messing up help output
    for f in "$out"/bin/*; do
      if head -n1 "$f" | grep -Eq '^#!.*(env +bash|/bin/bash)$'; then
        sed -i "2i export PATH=\"$runtimePath:\$PATH\"" "$f"
      fi
    done

    runHook postInstall
  '';

  meta = {
    description = "CLI tools from adzerk/cli-tools/micha";
    platforms = lib.platforms.unix;
  };
}
