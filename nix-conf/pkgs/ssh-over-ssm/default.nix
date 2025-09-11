{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  makeWrapper,
  awscli2,
  coreutils,
  getent,
  gnugrep,
  openssh,
  procps,
}:

stdenvNoCC.mkDerivation {
  pname = "ssh-over-ssm";
  version = "0-unstable-2023-06-11";

  src = fetchFromGitHub {
    owner = "elpy1";
    repo = "ssh-over-ssm";
    rev = "e439729a80dd65990bd3bf7b96f4a180dc93c7e3";
    hash = "sha256-99+tTlIHnithz3M5WNVcGznm9WxwW0G5JlxbEgTA1Kg=";
  };

  doBuild = false;

  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    runHook preInstall

    install -D ssh-ssm.sh $out/bin/ssh-ssm.sh
    wrapProgram $out/bin/ssh-ssm.sh \
      --prefix PATH : ${
        lib.makeBinPath [
          awscli2
          coreutils
          getent
          gnugrep
          openssh
          procps
        ]
      }

    runHook postInstall
  '';

  meta = with lib; {
    description = "SSH over AWS SSM";
    homepage = "https://github.com/elpy1/ssh-over-ssm";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
