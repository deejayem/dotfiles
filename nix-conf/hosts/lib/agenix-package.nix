{ lib, pkgs, inputs }:
(inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
  ageBin = lib.getExe pkgs.rage;
}).overrideAttrs
  (old: {
    # Don't append an extra -o every time (fails with rage). Issue closed as
    # "Not planned" in agenix: https://github.com/ryantm/agenix/issues/272
    installPhase = old.installPhase + ''
      substituteInPlace $out/bin/agenix --replace-fail \
        'DEFAULT_DECRYPT+=(-o "''${CLEARTEXT_FILE}")' \
        'local DEFAULT_DECRYPT=("''${DEFAULT_DECRYPT[@]}" -o "''${CLEARTEXT_FILE}")'
    '';
  })
