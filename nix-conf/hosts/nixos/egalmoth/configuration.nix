{ pkgs, ... }:
{
  imports = [
    ../modules/base.nix
    ./hardware-configuration.nix
  ];

  networking.useDHCP = false;
  networking.interfaces.enp45s0.useDHCP = true;
  networking.interfaces.wlp46s0.useDHCP = true;

  boot.kernelParams = [ "intel_pstate=enable" ];

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

  services.udev.packages = [
    (pkgs.writeTextFile {
      name = "epson_udev";
      text = ''
        ATTRS{idVendor}=="04b8", ATTRS{idProduct}=="084a", MODE="0664", GROUP="lp", ENV{libsane_matched}="yes"
      '';

      destination = "/etc/udev/rules.d/99-printer.rules";
    })
    (pkgs.writeTextFile {
      name = "iwlwifi_udev";
      text = ''
        SUBSYSTEM=="pci", KERNEL=="[0000:2e:00.0]", ATTR{d3cold_allowed}="0"
      '';

      destination = "/etc/udev/rules.d/99-iwlwifi.rules";
    })
  ];
  boot.extraModprobeConfig = "options iwlwifi disable_clkreq=y disable_aspm_l1=y disable_aspm_l1ss=y";

  system.stateVersion = "21.05";
}
