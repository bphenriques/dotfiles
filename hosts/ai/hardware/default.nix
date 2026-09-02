{ pkgs, private, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  hardware.enableRedistributableFirmware = true;

  boot = {
    kernelParams = [ "ttm.pages_limit=29360128" ];  # Set 112GiB for the iGPU
    blacklistedKernelModules = [
      "mt7925e"        # WiFi
      "btusb"          # Takes btrtl/btintel/btmtk/btbcm with it
      "bluetooth"
      "snd_hda_intel"  # No audio sink on a headless box
    ];
  };

  networking = {
    useDHCP = false;
    useNetworkd = true;
  };

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
    # Single consumer NVMe holding the OS and every model: short test weekly, long monthly.
    smartd = {
      enable = true;
      defaults.autodetected = "-a -s (S/../../7/03|L/../15/./04)";
    };
  };

  environment.systemPackages = [
    pkgs.ethtool
    pkgs.nvme-cli
    pkgs.pciutils
    pkgs.rocmPackages.rocm-smi
  ];
}
