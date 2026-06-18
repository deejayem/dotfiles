{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "datadog-pup";
  version = "1.1.0";

  src = fetchFromGitHub {
    owner = "DataDog";
    repo = "pup";
    rev = "v${finalAttrs.version}";
    hash = "sha256-Q92ulh4Sv177te2/xu0nac5NqwHO54EEwHa97i439mY=";
  };

  cargoHash = "sha256-ZwS8fgjPr/UcAmItpl3qXUvlAdVz8rWQrXskeubC5t0=";

  # Upstream's test build is broken (src/commands/auth.rs references an
  # undefined `token` function), so the binary builds but `cargo test` fails.
  doCheck = false;

  meta = {
    description = "CLI giving AI agents access to Datadog's observability platform";
    homepage = "https://github.com/DataDog/pup";
    license = lib.licenses.asl20;
    mainProgram = "pup";
    platforms = lib.platforms.unix;
  };
})
