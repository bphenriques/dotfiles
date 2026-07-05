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

  # bond0 opts back into DHCP despite the global useDHCP=false, so dhcpcd runs; noipv4ll
  # stops it assigning a useless 169.254.x.x link-local while the slow bond comes up.
  networking.dhcpcd.extraConfig = "noipv4ll";
  systemd.services.dhcpcd = {
    after = [ "sys-subsystem-net-devices-bond0.device" ];
    wants = [ "sys-subsystem-net-devices-bond0.device" ];
    serviceConfig.Slice = "critical.slice";
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
  # Hardware watchdog needs no config here: intel_oc_wdt auto-probes as /dev/watchdog and is
  # armed by RuntimeWatchdogSec in the headless profile (iTCO_wdt would just sit inactive).
  services.thermald.enable = true;      # Intel thermal daemon
  systemd.oomd.enable = true;           # Kill services under memory pressure before kernel OOM

  # Resource policy: throttle thermally-intensive media services into a capped slice; give critical
  # services their own high-priority slice. (dhcpcd joins critical.slice in its unit block above.)
  systemd.slices = {
    throttled.sliceConfig = {
      AllowedCPUs = "1-2";  # cores 0,3 reserved for system/critical (core 0 handles timer/boot interrupts)
      CPUQuota = "150%";    # hard cap prevents turbo heat-soak on the passively-cooled N150
      CPUWeight = 20;
      MemoryHigh = "16G";
      MemoryMax = "20G";
    };
    critical.sliceConfig.CPUWeight = 1000;
  };
  systemd.services.jellyfin.serviceConfig.Slice = "throttled.slice";
  # nixpkgs' immich module pins its own system-immich.slice, so force ours over it.
  systemd.services.immich-server.serviceConfig.Slice = lib.mkForce "throttled.slice";
  systemd.services.immich-machine-learning.serviceConfig.Slice = lib.mkForce "throttled.slice";
  systemd.services.sshd.serviceConfig.Slice = "critical.slice";

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
