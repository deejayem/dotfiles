{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchurl,
  cmake,
  rapidjson,
  substitute,
  libb64,
  versionCheckHook,
}:
let
  mpack = fetchurl {
    url = "https://github.com/ludocode/mpack/archive/df17e83f0fa8571b9cd0d8ccf38144fa90e244d1.tar.gz";
    hash = "sha256-hyiXygbAHnNgF4TIg+DemBvtdBnSgJ7fAhknVuL+T/c=";
  };
in
stdenv.mkDerivation (finalAttrs: {
  pname = "msgpack-tools";
  version = "0.6";

  src = fetchFromGitHub {
    owner = "ludocode";
    repo = "msgpack-tools";
    rev = "v${finalAttrs.version}";
    hash = "sha256-RT85vw6QeVkuNC2mtoT/BJyU0rdQVfz6ZBJf+ouY8vk=";
  };

  nativeBuildInputs = [
    cmake
  ];

  buildInputs = [
    rapidjson
    libb64
  ];

  patches = [
    (substitute {
      src = ./use-nix-deps.patch;
      substitutions = [
        "--subst-var-by"
        "rapidjson"
        "${rapidjson}"
        "--subst-var-by"
        "libb64"
        "${libb64}"
      ];
    })
  ];

  postUnpack = ''
    mkdir $sourceRoot/contrib
    cp ${mpack} $sourceRoot/contrib/mpack-df17e83f0fa8571b9cd0d8ccf38144fa90e244d1.tar.gz
  '';

  nativeInstallCheckInputs = [ versionCheckHook ];
  versionCheckProgram = "${placeholder "out"}/bin/json2msgpack";
  versionCheckProgramArg = "-v";
  doInstallCheck = true;

  meta = {
    description = "Command-line tools for converting between MessagePack and JSON";
    homepage = "https://github.com/ludocode/msgpack-tools";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
    maintainers = [ ];
  };
})
