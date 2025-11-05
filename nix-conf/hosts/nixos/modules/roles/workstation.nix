{ pkgs, ... }:
{
  imports = [
    ../services/gnome-keyring.nix
    ../services/greeter.nix
    ../services/pipewire.nix
    ../services/power.nix
    ../services/printing.nix
    ../services/touchpad.nix
  ];

  users.users.djm.extraGroups = [
    "networkmanager"
    "scanner"
    "lp"
    "cdrom"
    "disk"
    "input"
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.graphics.enable = true;

  networking.networkmanager.enable = true;

  services.dbus.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # This is installed by home-manager, but without programs.sway.enable = true
  # we cannot unlock swaylock
  programs.sway = {
    enable = true;
    package = null;
    extraPackages = [ ];
  };

  programs.xwayland.enable = true;

  fonts.packages = with pkgs; [
    corefonts
    iosevka-bin
    jetbrains-mono
    meslo-lgs-nf
    aporetic
  ];

  environment.systemPackages = with pkgs; [
    acpi
    acpitool
    alsa-utils
    firefox
    foot
    ghostscript
    imagemagick
    lm_sensors
    playerctl
    rofi
    sway
    vdhcoapp
    ungoogled-chromium
    wayland
    wl-clipboard
    wget
    xurls
    zoom-us
  ];
}
