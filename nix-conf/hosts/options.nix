{ lib, ... }:
{
  options.host = {
    role = lib.mkOption {
      type = lib.types.enum [
        "workstation"
        "server"
      ];
      description = "Host role";
    };
    org = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Organization for this host";
    };
    private = lib.mkOption {
      readOnly = true;
      type = lib.types.attrs;
      description = "PII/sensitive data loaded at eval time (ends up in the store)";
    };
  };
}
