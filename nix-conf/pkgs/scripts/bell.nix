{ pkgs }:

# Ring the terminal bell.
#
# Writing to /dev/tty only works from a process that has a controlling
# terminal. A process started detached from one - a hook, a daemon, a
# background job - gets "Device not configured" instead, and the bell is
# silently lost. So walk up the process tree to the nearest ancestor that
# does own a tty and write the bell to that device directly.
pkgs.writeShellScriptBin "bell" ''
  set -u

  pid=$PPID

  # Four levels is plenty: the terminal is normally the parent or grandparent.
  for _ in 1 2 3 4; do
    read -r dev parent <<< "$(ps -o tty=,ppid= -p "$pid" 2>/dev/null)"

    case "$dev" in
      # No controlling terminal on this process; ps prints "?" on Linux and
      # "??" on Darwin. Keep climbing.
      "" | "?" | "??")
        [ -n "$parent" ] || break
        pid=$parent
        ;;
      # ps reports the device relative to /dev ("ttys006", "pts/3").
      *)
        printf '\a' > "/dev/$dev" 2>/dev/null && exit 0
        break
        ;;
    esac
  done

  # Nothing in the tree owns a terminal. Fall back to stderr, which rings if
  # it happens to be a terminal and is a harmless no-op otherwise.
  printf '\a' >&2
  exit 0
''
