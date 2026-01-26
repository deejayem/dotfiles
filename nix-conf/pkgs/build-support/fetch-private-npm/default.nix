{ ... }:

{
  owner,
  name,
  version,
  downloadId,
  narHash ? "",
  registry,
}:

let
  registries = {
    github = "https://npm.pkg.github.com/download/@${owner}/${name}/${version}/${downloadId}";
  };
  url = registries.${registry} or (throw "Unknown npm registry: ${registry}");
in
# Uses builtins.fetchTree as it supports netrc (nix.settings.netrc-file)
builtins.fetchTree {
  type = "tarball";
  inherit url narHash;
}
