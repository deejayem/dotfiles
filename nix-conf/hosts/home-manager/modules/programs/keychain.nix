{ lib, pkgs, ... }:
{
  programs.keychain = {
    enable = lib.mkIf pkgs.stdenv.isLinux true;
    agents = [
      "ssh"
      "gpg"
    ];
    keys = [
      "id_rsa"
      "id_ed25519"
      "C171251002C200F2"
    ];
    #  extraFlags = [ "--quiet" "--ignore-missing" ];
  };
}
