{ pkgs, ... }:
{
  services.xserver.enable = true;
  services.xserver.exportConfiguration = true;
  services.xserver.displayManager.lightdm.greeters.slick.enable = true;
  services.xserver.xkb.layout = "gb";
  services.displayManager.sessionPackages = [ pkgs.sway ];
  services.displayManager.defaultSession = "sway";

  programs.xwayland.enable = true;

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  services.dbus.enable = true;

  programs.seahorse.enable = true;
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.login.enableGnomeKeyring = true;
  security.pam.services.passwd.enableGnomeKeyring = true;
}
