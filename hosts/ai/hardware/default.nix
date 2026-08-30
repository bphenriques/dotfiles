{ pkgs, private, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  hardware.enableRedistributableFirmware = true;

  boot = {
    # TTM caps how much system RAM the iGPU may pin, and defaults to half. 112 GiB in 4KiB pages,
    # leaving ~11 GiB for the OS. IOMMU needs no kernel param: the BIOS already enables it.
    kernelParams = [ "ttm.pages_limit=29360128" ];

    # Headless and wired: the radios and the HDA controller are dead weight, and the WiFi chip
    # otherwise shows up as a hwmon sensor the temperature alerts would have to filter out.
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

  # Only the cabled RTL8127 is configured; the second one stays down to wire by hand.
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

  services.fstrim.enable = true;

  environment.systemPackages = [
    pkgs.ethtool
    pkgs.nvme-cli
    pkgs.pciutils
    pkgs.rocmPackages.rocm-smi
  ];
}
