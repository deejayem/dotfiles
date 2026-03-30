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
        echo ""
        echo "Options:"
        echo "  -w, --workspace DIR   Workspace directory (default: \$PWD)"
        echo ""
        echo "Examples:"
        echo "  sandbox opencode"
        echo "  sandbox claude"
        echo "  sandbox codex"
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

  # ── Tool state directory ────────────────────────────────
  # Known tools get their canonical state dir; everything
  # else gets a generic fallback.
  case "$TOOL" in
    opencode) TOOL_STATE="''${XDG_DATA_HOME:-$HOME/.local/share}/opencode" ;;
    claude)   TOOL_STATE="$HOME/.claude" ;;
    codex)    TOOL_STATE="$HOME/.codex" ;;
    *)        TOOL_STATE="$HOME/.local/share/sandbox" ;;
  esac
  mkdir -p "$TOOL_STATE"

  # ── Raise fd limit (coding agents use many) ─────────────
  ulimit -n 2147483646 2>/dev/null || true

  # ── Launch under sandbox-exec ───────────────────────────
  exec sandbox-exec \
    -D HOME="$HOME" \
    -D WORKSPACE="$WORKSPACE" \
    -D TOOL_STATE="$TOOL_STATE" \
    -f "${sandboxSb}" \
    "$TOOL" "$@"
''
