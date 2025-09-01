{
  lib,
  stdenv,
  fetchFromGitHub,
  glibc,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "json-table";
  version = "2.0.0";

  src = fetchFromGitHub {
    owner = "micha";
    repo = "json-table";
    rev = finalAttrs.version;
    hash = "sha256-WBD5lB4EJHqBcYANAIKtBLnf0r9DL+wGQvlmc+5KTps=";
  };

  makeFlags = [
    "PREFIX=${placeholder "out"}"
    "CFLAGS=-Wno-int-conversion"
    "LDFLAGS=" # Disable static linking
  ];

  meta = with lib; {
    description = "Transform JSON data into tables";
    homepage = "https://github.com/micha/json-table";
    license = licenses.epl10;
    platforms = platforms.unix;
  };
})
