{
  lib,
  buildNpmPackage,
  fetchFromPrivateGitHub,
  lmdb,
  nodejs,
  ...
}:
buildNpmPackage {
  pname = "lmdb-cli";
  version = "0.12.0";

  src = fetchFromPrivateGitHub {
    owner = "adzerk";
    repo = "lmdb-cli";
    rev = "ed7595a58289850c7d126df86e400d9b6d60019b";
    narHash = "sha256-DC+cWBWy0YEmvFRnP+EqYl+bQ1eklLkDWZxPJIsbJzg=";
  };

  inherit nodejs;

  npmDepsHash = "sha256-ote/3y91Ari9YkqzRHhrOO7ftBIxkLU4YS2MV+Z2dIM=";

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
    platforms = lib.platforms.unix;
  };
}
