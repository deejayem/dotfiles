{
  lib,
  python3Packages,
  fetchFromGitHub,
}:

python3Packages.buildPythonApplication {
  pname = "codex-transcript-viewer";
  version = "0-unstable-2026-02-27";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "masonc15";
    repo = "codex-transcript-viewer";
    rev = "2d0df3f1ce8b41c085c181c8df7aebcfb6ef9b0d";
    hash = "sha256-ZLsAkyYK45lvxXmlOGwkkESxIngM3WcN4Ta1NSZnB80=";
  };

  build-system = [ python3Packages.hatchling ];

  nativeCheckInputs = [ python3Packages.unittestCheckHook ];

  # The tests directory is not a package, so discovery has to be pointed at it.
  unittestFlags = [
    "--start-directory"
    "tests"
  ];

  pythonImportsCheck = [ "codex_transcript_viewer" ];

  meta = {
    description = "Convert Codex CLI JSONL session transcripts to self-contained HTML viewers";
    homepage = "https://github.com/masonc15/codex-transcript-viewer";
    license = lib.licenses.mit;
    mainProgram = "codex-transcript-viewer";
    platforms = lib.platforms.unix;
  };
}
