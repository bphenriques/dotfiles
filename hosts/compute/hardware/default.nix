{ lib, pkgs, config, private, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  # Disk
  services.fstrim.enable = true;  # Weekly TRIM for NVMe longevity
  services.smartd.enable = true;  # Disk health

  # RAM: ~8GB on 32GB RAM. OOM safety net without NVMe wear
  zramSwap = {
    enable = true;
    memoryPercent = 25;
    algorithm = "lz4";  # Cheaper than zstd
  };

  # Graphics
  hardware.graphics = {
    enable = true;
    extraPackages = [
      pkgs.intel-media-driver    # VAAPI driver (iHD). Only driver supporting Alder Lake-N
      pkgs.vpl-gpu-rt            # oneVPL GPU runtime (required for QSV)
      pkgs.intel-compute-runtime # OpenCL runtime. Required for HDR->SDR tonemapping (Jellyfin)
    ];
  };
  environment.systemPackages = [ pkgs.intel-gpu-tools ];
  boot.kernelParams = [
    "i915.enable_guc=3"                 # Enable GuC/HuC firmware for better media scheduling
    "block.events_dfl_poll_msecs=0"     # Disable removable media polling (no optical drives)
  ];

  # bond0 over enp1s0/enp2s0 (active-backup) via systemd-networkd. The bond MAC is pinned to
  # enp1s0's — the DHCP reservation is keyed on it (private.settings.network.bond0MAC).
  networking.useDHCP = false;
  networking.useNetworkd = true;
  systemd.network = {
    netdevs."10-bond0" = {
      netdevConfig = { Kind = "bond"; Name = "bond0"; MACAddress = private.settings.network.bond0MAC; };
      bondConfig = { Mode = "active-backup"; MIIMonitorSec = "100ms"; };  # link monitoring, then failover on cable pull
    };
    networks = {
      "10-enp1s0" = { matchConfig.Name = "enp1s0"; networkConfig.Bond = "bond0"; };
      "10-enp2s0" = { matchConfig.Name = "enp2s0"; networkConfig.Bond = "bond0"; };
      "10-bond0" = {
        matchConfig.Name = "bond0";
        networkConfig = {
          DHCP = "ipv4";
          IPv6AcceptRA = true; # keep the routable global IPv6 (RA)
        };
      };
    };

    # WoL by original name: enslaved slaves read the bond MAC, so can't match on address.
    links = {
      "10-enp1s0" = { matchConfig.OriginalName = "enp1s0"; linkConfig.WakeOnLan = "magic"; };
      "10-enp2s0" = { matchConfig.OriginalName = "enp2s0"; linkConfig.WakeOnLan = "magic"; };
    };
  };

  # Power management
  powerManagement = {
    powertop.enable = true;            # Auto-tune power settings at boot
    cpuFreqGovernor = "powersave";     # Favor low frequencies, still allows turbo when needed
  };

  # UPS NUT client: For credentials update `cat /etc/ups/upsd.users` on the Synology NAS and restart `synosystemctl restart ups-usb`
  power.ups = {
    enable = true;
    mode = "netclient";
    upsmon.monitor.synology = {
      system = "ups@${config.custom.fleet.lan.hosts.bruno-home-nas}";
      powerValue = 1;
      user = "compute";
      passwordFile = config.sops.secrets."upsmon/password".path;
      type = "secondary";
    };
  };
  sops.secrets."upsmon/password" = {
    mode = "0440";   # NUT exporter reads via SupplementaryGroups=keys
    group = "keys";
  };

  # Thermal & stability
  services.thermald.enable = true;      # Intel thermal daemon
  systemd.oomd.enable = true;           # Kill services under memory pressure before kernel OOM
  systemd.slices = {
    throttled.sliceConfig = {
      AllowedCPUs = "1-2";              # cores 0,3 reserved for system/critical (core 0 handles timer/boot interrupts)
      CPUQuota = "150%";                # hard cap prevents turbo heat-soak on the passively-cooled N150
      CPUWeight = 20;
      MemoryHigh = "16G";
      MemoryMax = "20G";
    };
    critical.sliceConfig.CPUWeight = 1000; # Reserved for critical services
  };

 # Misc
  hardware.enableRedistributableFirmware = true;
  boot.blacklistedKernelModules = [
    "iwlwifi"    # WiFi (always on Ethernet)
    "btusb"      # Bluetooth USB
    "bluetooth"  # Bluetooth stack
    "i8042"      # PS/2 keyboard controller
    "serio"      # PS/2 serial I/O
  ];
}
