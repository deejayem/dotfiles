{
  writeShellScriptBin,
  ...
}:

let
  sandboxSb = ./sandbox.sb;
in
writeShellScriptBin "sandbox" ''
  set -euo pipefail

  WORKSPACE="$PWD"

  # ── Parse flags ─────────────────────────────────────────
  while [ $# -gt 0 ]; do
    case "$1" in
      -w|--workspace) WORKSPACE="$2"; shift 2 ;;
      -h|--help)
        echo "Usage: sandbox [-w DIR] <command> [args...]"
        echo ""
        echo "Run a command under a macOS Seatbelt sandbox."
        echo "Designed for OpenCode but works with any command."
        echo ""
        echo "Options:"
        echo "  -w, --workspace DIR   Workspace directory (default: \$PWD)"
        echo ""
        echo "Examples:"
        echo "  sandbox opencode"
        echo "  sandbox -w ~/src/myproject opencode"
        echo "  sandbox make test"
        exit 0
        ;;
      *) break ;;
    esac
  done

  if [ $# -eq 0 ]; then
    echo "Usage: sandbox [-w DIR] <command> [args...]" >&2
    echo "Run 'sandbox --help' for more information." >&2
    exit 1
  fi

  TOOL="$1"; shift

  # ── Resolve workspace to absolute path ──────────────────
  WORKSPACE="$(cd "$WORKSPACE" && pwd)"

  # ── Tool state directories ───────────────────────────────
  # ~/.local covers share/<tool>, state/<tool>/locks, etc.
  # ~/.cache covers opencode caches and version checks.
  TOOL_STATE="$HOME/.local"
  CACHE_DIR="$HOME/.cache"
  mkdir -p "$TOOL_STATE" "$CACHE_DIR"

  # ── Raise fd limit (coding agents use many) ─────────────
  ulimit -n 2147483646 2>/dev/null || true

  # ── Launch under sandbox-exec ───────────────────────────
  # cd into workspace BEFORE exec so that getcwd() succeeds
  # inside the sandbox (PWD outside allowed paths causes hangs).
  cd "$WORKSPACE"
  exec sandbox-exec \
    -D HOME="$HOME" \
    -D WORKSPACE="$WORKSPACE" \
    -D TOOL_STATE="$TOOL_STATE" \
    -D CACHE_DIR="$CACHE_DIR" \
    -f "${sandboxSb}" \
    "$TOOL" "$@"
''
