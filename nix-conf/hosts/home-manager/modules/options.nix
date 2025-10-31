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
  };
}
