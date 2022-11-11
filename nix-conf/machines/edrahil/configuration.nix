{ config, pkgs,... }: {
  imports = [
    ./hardware-configuration.nix
    ./network-configuration.nix
  ];

  boot.cleanTmpDir = true;
  zramSwap.enable = true;

  networking.hostName = "edrahil";
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 113 2222 ];
  };

  services.openssh = {
    enable = true;
    ports = [ 2222 ];
    permitRootLogin = "no";
    passwordAuthentication = false;
    allowSFTP = true;
    kbdInteractiveAuthentication = false;
    extraConfig = ''
      #AllowTcpForwarding yes
      X11Forwarding no
      AllowAgentForwarding no
      AllowStreamLocalForwarding no
      AuthenticationMethods publickey
      AllowUsers djm
    '';
  };
  services.sshguard.enable = true;
  services.oidentd.enable = true;

  services.locate = {
    enable = true;
    locate = pkgs.plocate;
    localuser = null;
  };

  users.users.djm =
   { isNormalUser = true;
     home = "/home/djm";
     description = "David Morgan";
     extraGroups = [ "wheel" "plocate" ];
     shell = pkgs.zsh;
     openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCurCpxZCHtByB5wXzsjTXwMyDSB4+B8rq5XY6EGss58NwD8jc5cII4i+QUbCOGTiAggSZUSC9YIP24hjpOeNT/IYs5m7Qn1B9MtBAiUSrIYew8eDwnMLlPzN+k2x9zCrJeCHIvGJaFHPXTh1Lf5Jt2fPVGW9lksE/XUVOe6ht4N/b+nqqszXFhc8Ug6le2bC1YeTCVEf8pjlh/I7DkDBl6IB8uEXc3X2vxxbV0Z4vlBrFkkAywcD3j5VlS/QYfBr4BICNmq/sO3fMkbMbtAPwuFxeL4+h6426AARQZiSS0qVEc8OoFRBVx3GEH5fqVAWfB1geyLzei22HbjUcT9+xN davidmo@gendros"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK9UDTaVnUOU/JknrNdihlhhGOk53LmHq9I1ASri3aga djm@gaius"
     ];
   };

   security.sudo.extraConfig = ''
     djm ALL=(ALL) NOPASSWD: ALL
   '';
   security.doas = {
     enable = true;
     extraRules = [ { users = [ "djm" ]; noPass = true; keepEnv = true; } ];
   };

   environment.systemPackages = with pkgs; [
     #procmail
     git
     vim
     wget
   ];
   environment.variables = { EDITOR = "vim"; VISUAL = "vim"; };

   nix.trustedUsers = [ "root" "djm" ];

   system.stateVersion = "22.05";

}