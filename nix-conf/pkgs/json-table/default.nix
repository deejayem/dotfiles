{
  lib,
  stdenv,
  fetchFromGitHub,
  glibc,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "json-table";
  version = "4.3.3";

  src = fetchFromGitHub {
    owner = "micha";
    repo = "json-table";
    rev = finalAttrs.version;
    hash = "sha256-VvoGFyYx7NfAf0sb9iw7dsK1ZLFfKv2kL/Msa72gFdo=";
  };

  makeFlags = [
    "PREFIX=${placeholder "out"}"
    "CFLAGS=-Wno-strict-prototypes"
    "LDFLAGS=" # Disable static linking
  ];

  meta = with lib; {
    description = "Transform JSON data into tables";
    homepage = "https://github.com/micha/json-table";
    license = licenses.epl10;
    platforms = platforms.unix;
  };
})
