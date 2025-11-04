{ ... }:

# Lazily included flakes. not ideal, but this allows flakes to be used without adding them to
# the main flake inputs, which takes up disk space, even on systems where they aren't being used.
final: prev: {
  mac-app-util =
    (builtins.getFlake "github:hraban/mac-app-util?rev=8414fa1e2cb775b17793104a9095aabeeada63ef&hash=sha256-ziR5eQGqRWhW8tf8r0TIplaqNt+HXu1G1X41LUr4IYo=")
    .packages.${prev.pkgs.stdenv.hostPlatform.system}.default;
}
