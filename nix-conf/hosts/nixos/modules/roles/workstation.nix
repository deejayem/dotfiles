{ pkgs, ... }:
{
  imports = [
    ../pipewire.nix
    ../power.nix
    ../printing.nix
    ../touchpad.nix
    ../wayland.nix
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

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [ xorg.libxcb ];

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
