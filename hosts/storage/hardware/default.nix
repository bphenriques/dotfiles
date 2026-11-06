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
      # The 15th at 07:00 avoids the monthly scrub and nightly backup on the same spindles.
      defaults.autodetected = "-a -s (S/../../7/02|L/../15/./07)";
    };
  };

  zramSwap = {
    enable = true;
    memoryPercent = 25;
    algorithm = "lz4";
  };

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
