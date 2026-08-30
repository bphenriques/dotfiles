{ pkgs, private, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  hardware.enableRedistributableFirmware = true;

  networking = {
    useDHCP = false;
    useNetworkd = true;
  };

  # Only the RTL8126A 5GbE port is configured; the i226-V stays down as a fallback to wire by hand.
  systemd.network = {
    networks."10-lan" = {
      matchConfig.MACAddress = private.settings.network.lanMAC;
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = true;
      };
    };
    links."10-lan" = {
      matchConfig.MACAddress = private.settings.network.lanMAC;
      linkConfig.WakeOnLan = "magic";
    };
  };

  services = {
    fstrim.enable = true;
    smartd = {
      enable = true;
      # Long test on the 15th at 07:00: the 1st at 03:00 collided with the monthly scrub window and
      # the nightly backup, all on the same two spindles.
      defaults.autodetected = "-a -s (S/../../7/02|L/../15/./07)";
    };
    # No thermald: it exits at startup here, and drive temperature is covered by the smartctl exporter.
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
    "mt7921e"    # WiFi (always wired)
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
