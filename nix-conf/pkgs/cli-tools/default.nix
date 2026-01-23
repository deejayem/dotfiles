{
  lib,
  stdenvNoCC,
}:

stdenvNoCC.mkDerivation {
  pname = "cli-tools";
  version = "0-unstable-2025-04-10";

  # Use fetchTree as it supports netrc (nix.settings.netrc-file)
  src = builtins.fetchTree {
    type = "git"; # type = "github" currently fails with a 404
    url = "https://github.com/adzerk/cli-tools.git";
    rev = "63b1fd6fb6c073d2f49d9a7a38e04988e787c45d";
    narHash = "sha256-0lE/pUTxIN4E8eJTncNAfeD/AMEgzzJQ5kq3gwpUfBw=";
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
