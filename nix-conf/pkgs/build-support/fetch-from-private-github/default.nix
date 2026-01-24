{ ... }:
{
  owner,
  repo,
  rev,
  narHash ? "",
  ...
}:
# Use fetchTree as it supports netrc (nix.settings.netrc-file)
builtins.fetchTree {
  type = "git"; # type = "github" currently fails with a 404
  url = "https://github.com/${owner}/${repo}.git";
  inherit narHash rev;
}
