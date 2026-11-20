{
  lib,
  stdenvNoCC,
  fetchurl,
  autoPatchelfHook,
  versionCheckHook,
}:

let
  sources = {
    aarch64-darwin = {
      platform = "darwin-arm64";
      hash = "sha256-6VuShal/PXmkrhEiJkpD9yUidAlE3i3I/xuSN2QYkm0=";
    };
    aarch64-linux = {
      platform = "linux-arm64";
      hash = "sha256-ms6gEn6bwS+Gh9qBQ0/mHkV4rn3eVW3eYm0+yqtFNQw=";
    };
    x86_64-linux = {
      platform = "linux-x64";
      hash = "sha256-KJ6AT/rlT+lY0L4+qDdNyAan40HTzzTl/sqBrQwfQb8=";
    };
  };
  source = sources.${stdenvNoCC.hostPlatform.system};
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "twg-cli";
  version = "1.4.6";

  src = fetchurl {
    url = "https://teamwork-graph.atlassian.com/cli/twg-${source.platform}-v${finalAttrs.version}";
    inherit (source) hash;
  };

  dontUnpack = true;
  dontBuild = true;
  # Stripping removes the JavaScript payload embedded in the Bun executable.
  dontStrip = true;

  nativeBuildInputs = lib.optionals stdenvNoCC.hostPlatform.isLinux [ autoPatchelfHook ];

  # Bun's localeCompare uses the system ICU data on macOS.
  sandboxProfile = lib.optionalString stdenvNoCC.hostPlatform.isDarwin ''
    (allow file-read* (subpath "/usr/share/icu"))
  '';

  installPhase = ''
    runHook preInstall

    install -Dm755 "$src" "$out/bin/twg"

    runHook postInstall
  '';

  nativeInstallCheckInputs = [ versionCheckHook ];
  doInstallCheck = true;
  versionCheckProgramArg = "--version";

  meta = {
    description = "Command-line interface for Atlassian Teamwork Graph";
    homepage = "https://github.com/atlassian/twg-cli";
    license = lib.licenses.unfree;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    mainProgram = "twg";
    platforms = lib.attrNames sources;
  };
})
