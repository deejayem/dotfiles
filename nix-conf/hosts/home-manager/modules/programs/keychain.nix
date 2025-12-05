{ ... }:
{
  programs.keychain = {
    enable = true;
    keys = [
      "id_rsa" # TODO: confirm this is no longer used and remove
      "id_ed25519"
      "C171251002C200F2"
    ];
  };
}
