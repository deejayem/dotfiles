{
  pkgs,
  role,
  ...
}:
{
  imports = [
    ../../options.nix
    ./users.nix
    ./services/ssh.nix
    ./roles/${role}.nix
  ];

  host.role = role;

  boot.tmp.cleanOnBoot = true;

  time.timeZone = "Europe/London";
  i18n.defaultLocale = "en_GB.UTF-8";

  programs.zsh.enable = true;

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    withRuby = false;
    withPython3 = false;
  };

  services.locate.enable = true;

  environment.systemPackages = with pkgs; [
    doas
    git
    rage
    wget
  ];

  nix.settings.trusted-users = [
    "root"
    "djm"
  ];
}
