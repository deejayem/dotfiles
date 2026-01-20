{
  buildNpmPackage,
  nodejs_20,
  lmdb,
  ...
}:

let
  lmdbCliRev = "dc331107dc374e047e21fb9262c51bf44f5b019d";

  # Use fetchTree as it supports netrc (nix.settings.netrc-file)
  lmdbCliSrc = builtins.fetchTree {
    type = "github";
    owner = "adzerk";
    repo = "lmdb-cli";
    rev = lmdbCliRev;
    narHash = "sha256-6oh+j7FmXM1PT9n0HgkpIeqnlTGpxmMHu2bfD633uz8=";
  };
in
buildNpmPackage {
  pname = "lmdb-cli";
  version = "1.0.0";
  src = lmdbCliSrc;

  nodejs = nodejs_20;

  npmDepsHash = "sha256-fF52CSlVOEuAlP19iyBe2daC86fBjY/jduQhc07k0Fw=";

  makeCacheWritable = true;
  dontNpmBuild = true;
  npmFlags = [ "--omit=dev" ];
  buildInputs = [ lmdb ];

  # Patch the lockfile to use https intead of ssh.
  postPatch = ''
    if [ -f package-lock.json ]; then
      substituteInPlace package-lock.json \
        --replace-fail "git+ssh://git@github.com/" "git+https://github.com/"
    fi
  '';

  meta = {
    description = "LMDB CLI tool";
    mainProgram = "lmdb-cli";
  };
}
