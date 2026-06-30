{ lib, pkgs, config, ... }:
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

  # Bonding: DHCP reservation uses bond0's MAC (inherited from enp1s0)
  networking.useDHCP = false;
  networking.bonds.bond0 = {
    interfaces = [ "enp1s0" "enp2s0" ];
    driverOptions = {
      mode = "active-backup";
      primary = "enp1s0";
    };
  };
  networking.interfaces.bond0.useDHCP = true;
  networking.interfaces.enp1s0.wakeOnLan.enable = true;
  networking.interfaces.enp2s0.wakeOnLan.enable = true;

  # bond0 is slow to start and to get the right IP we need it ready before starting dhcpcd. Otherwise: Otherwise, fall back to 169.254.x.x.
  networking.dhcpcd.extraConfig = "noipv4ll"; # FIXME: Does this even make sense if we have useHDCP set to false?
  systemd.services.dhcpcd = {
    after = [ "sys-subsystem-net-devices-bond0.device" ];
    wants = [ "sys-subsystem-net-devices-bond0.device" ];
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
  boot.kernelModules = [ "iTCO_wdt" ];  # Hardware watchdog: Intel TCO timer. Confirm with journalctl -b -g 'watchdog\|iTCO'
  services.thermald.enable = true;      # Intel thermal daemon
  systemd.oomd.enable = true;           # Kill services under memory pressure before kernel OOM

  # Resource control: dedicated slice for programs that may have thermally intensive and ensure critical services take priority
  selfhost.resourceControl.slices = {
    throttled.sliceConfig = {
      AllowedCPUs = "1-2";  # cores 0,3 reserved for system/critical (core 0 handles timer/boot interrupts)
      CPUQuota = "150%";    # Hard cap to prevent heating the CPU
      CPUWeight = 20;
      MemoryHigh = "16G";
      MemoryMax = "20G";
    };
    critical = {
      extraSystemdServices = [ "sshd" "dhcpcd" ];
      sliceConfig.CPUWeight = 1000;
    };
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
