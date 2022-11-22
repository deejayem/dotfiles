{ config, pkgs, lib, ... }:

let
  unstable = import <nixos-unstable> { };
in
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = ["intel_pstate=enable"];
  powerManagement = {
    enable = true;
    #cpuFreqGovernor = "powersave";
    powertop.enable = true;
  };
  services.thermald.enable = true;
  services.power-profiles-daemon.enable = false;
  services.tlp = {
    enable = true;
    settings = {
      TLP_ENABLE = 1;
      TLP_DEFAULT_MODE = "BAT";
      MAX_LOST_WORK_SECS_ON_AC = 15;
      MAX_LOST_WORK_SECS_ON_BAT = 60;
      CPU_HWP_ON_AC = "balance_performance";
      CPU_HWP_ON_BAT = "power";
      CPU_MAX_PERF_ON_BAT = 40;
      CPU_BOOST_ON_BAT = 0;
      SCHED_POWERSAVE_ON_AC = 0;
      SCHED_POWERSAVE_ON_BAT = 1;
      ENERGY_PERF_POLICY_ON_AC = "balance-performance";
      ENERGY_PERF_POLICY_ON_BAT = "power";
      DISK_DEVICES = "nvme0n1 sda";
      DISK_APM_LEVEL_ON_AC = "254 254";
      DISK_APM_LEVEL_ON_BAT = "128 128";
      SATA_LINKPWR_ON_AC = "med_power_with_dipm max_performance";
      SATA_LINKPWR_ON_BAT = "med_power_with_dipm min_power";
      PCIE_ASPM_ON_BAT = "powersave";
      WIFI_PWR_ON_AC = "off";
      WIFI_PWR_ON_BAT = "on";
      WOL_DISABLE = "Y";
      SOUND_POWER_SAVE_ON_AC = 0;
      SOUND_POWER_SAVE_ON_BAT = 1;
      RUNTIME_PM_ON_AC = "on";
      RUNTIME_PM_ON_BAT = "auto";
      USB_AUTOSUSPEND = 1;
      USB_BLACKLIST = "0bda:8153";
      RESTORE_DEVICE_STATE_ON_STARTUP = 1;
    };
  };

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;

  networking.hostName = "egalmoth"; # Define your hostname.
  networking.networkmanager.enable = true;

  time.timeZone = "Europe/London";

  networking.useDHCP = false;
  networking.interfaces.enp45s0.useDHCP = true;
  networking.interfaces.wlp46s0.useDHCP = true;

  services.xserver.enable = true;
  services.xserver.exportConfiguration = true;


  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.displayManager.sessionPackages = [ pkgs.sway ];
  #services.xserver.displayManager.defaultSession = "none+i3";
  services.xserver.displayManager.defaultSession = "sway";

  services.xserver.windowManager.i3.enable = true;
  services.upower.enable = true;

  services.xserver.layout = "gb";

  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint pkgs.hplipWithPlugin ];

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
     "hplip"
  ];

  hardware.sane.enable = true;

  services.udev.packages = [
    (pkgs.writeTextFile {
      name = "epson_udev";
      text = ''
        ATTRS{idVendor}=="04b8", ATTRS{idProduct}=="084a", MODE="0664", GROUP="lp", ENV{libsane_matched}="yes"
      '';

      destination = "/etc/udev/rules.d/99-printer.rules";
    })
  ];

  services.xserver.libinput = {
    enable = true;
    touchpad = {
      clickMethod = "clickfinger";
      naturalScrolling = true;
      #tappingButtonMap = "lrm";
      #tappingButtonMap = "lmr";
    };
  };

  services.dbus.enable = true;

  users.users.djm =
   { isNormalUser = true;
     description = "David Morgan";
     extraGroups = [ "wheel" "networkmanager" "scanner" "lp" "plocate" "cdrom" ];
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


  services.locate = {
    enable = true;
    locate = pkgs.plocate;
    localuser = null;
  };

  environment.systemPackages = with pkgs; [
    acpi
    acpitool
    bemenu
    #dbus-sway-environment
    firefox
    foot
    ghostscript
    git
    i3
    imagemagick
    lm_sensors
    playerctl
    rofi
    st
    sway
    vim
    wayland
    wayst
    wezterm
    wl-clipboard
    wget
    xclip
    xurls
    xst
  ];

  fonts.fonts = with pkgs; [
    unstable.iosevka-comfy.comfy
    iosevka-bin
    jetbrains-mono
    meslo-lgs-nf
  ];

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.vim.defaultEditor = true;

  services.openssh.enable = true;

  i18n.defaultLocale = "en_GB.UTF-8";

  system.stateVersion = "21.05"; # Did you read the comment?

}

