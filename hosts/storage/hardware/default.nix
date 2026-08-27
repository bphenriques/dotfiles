{ pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  hardware.enableRedistributableFirmware = true;

  services = {
    fstrim.enable = true;
    smartd = {
      enable = true;
      # Long test on the 15th at 07:00: the 1st at 03:00 collided with the monthly scrub window and
      # the nightly backup, all on the same two spindles.
      defaults.autodetected = "-a -s (S/../../7/02|L/../15/./07)";
    };
    # No thermald: this chassis is not a mobile platform, so it exits at startup. The thermal risk
    # here is drive temperature, not the CPU, and that is covered by the smartctl exporter.
  };

  zramSwap = {
    enable = true;
    memoryPercent = 25;
    algorithm = "lz4";
  };

  systemd.oomd.enable = true;

  # No powertop: its auto-tune enables USB autosuspend, and the UPS arrives on USB HID.
  # No cpuFreqGovernor either: intel_pstate already defaults to powersave here.

  boot.blacklistedKernelModules = [
    "mt7921e"    # WiFi (always wired); MediaTek here, unlike compute's iwlwifi
    "btusb"      # Takes btrtl/btintel/btmtk/btbcm with it
    "bluetooth"
  ];

  environment.systemPackages = [
    pkgs.ethtool
    pkgs.nvme-cli
    pkgs.pciutils
    pkgs.smartmontools
  ];
}
