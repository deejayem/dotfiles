{ config, pkgs, ... }:
let
  #secrets = builtins.extraBuiltins.readSops secrets.yaml;
  #plugs = (pkgs.nix-plugins.override { nix = pkgs.nixVersions.nix_2_24; }).overrideAttrs (o: {
  #  buildInputs = [pkgs.nixVersions.nix_2_24 pkgs.boost];
  #  patches = (o.patches or []) ++ [../../nix-plugins.patch];
  #});
  plugs = pkgs.nix-plugins.overrideAttrs (o: {
    #nix = pkgs.nixVersions.nix_2_24;
    #buildInputs = [pkgs.nixVersions.nix_2_24 pkgs.boost];
    buildInputs = [pkgs.nixVersions.latest pkgs.boost];
    patches = (o.patches or []) ++ [
      ../../nix-plugins.patch
      (pkgs.fetchpatch {
        url = "https://raw.githubusercontent.com/chayleaf/dotfiles/2f8865c3f5880dfc24bdd9d7ccf7e1b3880ba680/pkgs/nix-plugins-fix.patch";
        hash = "sha256-IHNlIhYfnwFfwD/FxPXxbcvOqnsH5/XjA3fOyuoGj5c=";
      })
    ];
  });
in
{
  imports = [ ./hardware-configuration.nix ];

  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = true;

  networking.hostName = "djmuk2";
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 113 ];
  };


  sops = {
    defaultSopsFile = builtins.path {
      path = ./secrets.yaml;
      name = "djmuk2-secrets.yaml";
    };
  };


  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
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

  services.locate.enable = true;

  # Emulate nix-sops. Technically an anti-pattern, but this isn't a real secret, and this has to be embedded here, as we cannot set a file path to read it from.
  # Populate/update with:
  # SOPS_AGE_KEY=$(doas ssh-to-age -private-key -i /etc/ssh/ssh_host_ed25519_key) sops -d --extract '["openiscsi_name"]' machines/djmuk2/secrets.yaml | doas tee /root/.config/secrets/openiscsi_name
  # TODO: comments
  services.openiscsi.enable = true;
  #services.openiscsi.name = (builtins.extraBuiltins.sopsFromYAML config.sops.defaultSopsFile).openiscsi_name;
  #services.openiscsi.name = secrets.openiscsi_name;
  services.openiscsi.name = "iqn.2015-12.com.oracleiaas:b729d5b6-d6b0-46cd-be60-820ec3023a16";
  #services.openiscsi.name = builtins.readFile /home/djm/dotfiles/machines/djmuk2/openiscsi_name;
  #services.openiscsi.enableAutoLoginOut = true;

  users.users.djm = {
    isNormalUser = true;
    home = "/home/djm";
    description = "David Morgan";
    extraGroups = [
      "wheel"
      "plocate"
    ];
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
    extraRules = [
      {
        users = [ "djm" ];
        noPass = true;
        keepEnv = true;
      }
    ];
  };

  programs.zsh.enable = true;

  programs.vim = {
    enable = true;
    defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
    nix-plugins
    #procmail
    git
    wget
    #plugs
    plugs
  ];

  nix.settings.trusted-users = [
    "root"
    "djm"
  ];
  #plugin-files = ${(pkgs.nix-plugins.override { nix = pkgs.nixVersions.nix_2_18; }).overrideAttrs (o: {
  #  buildInputs = [pkgs.nixVersions.nix_2_18 pkgs.boost];
  #  patches = (o.patches or []) ++ [../../nix-plugins.patch];
  #})}/lib/nix/plugins
  nix.extraOptions = ''
    plugin-files = ${plugs}/lib/nix/plugins
    extra-builtins-file = [ ../../lib/extra-builtins.nix ];
'';
  #nix = {
  #  settings = {
  #    plugin-files = "${pkgs.nix-plugins}/lib/nix/plugins";
  #    extra-builtins-file = [ ../../lib/extra-builtins.nix ];
  #  };
  #};
  nix.optimise.automatic = true;
  nix.optimise.dates = [ "03:00" ];

  i18n.defaultLocale = "en_GB.UTF-8";

  system.stateVersion = "22.05";
}
