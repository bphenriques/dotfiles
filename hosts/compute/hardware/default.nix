{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix  # Output of nixos-generate-config --root /mnt
  ];

  hardware.enableRedistributableFirmware = true;  # Misc drivers
  services.fwupd.enable = true;                   # Updates firmwares: `fwupdmgr`.

  # Disk
  services.fstrim.enable = true;  # Weekly TRIM for NVMe longevity
  services.smartd.enable = true;  # Disk health — alerts via smartctl_exporter + Prometheus (see ../monitoring/)

  # RAM: ~8GB on 32GB RAM - OOM safety net without NVMe wear
  zramSwap = {
    enable = true;
    memoryPercent = 25;  #
    algorithm = "zstd";
  };

  # Graphics
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver   # primary VAAPI driver (iHD)
      intel-vaapi-driver   # fallback
    ];
  };
  environment.systemPackages = [ pkgs.intel-gpu-tools ]; # iGPU monitoring (intel_gpu_top)

  # Bonding: only bond0 gets DHCP, physical interfaces stay silent
  # Router DHCP reservation should use bond0's MAC (inherited from enp1s0)
  networking.useDHCP = false;
  networking.bonds.bond0 = {
    interfaces = [ "enp1s0" "enp2s0" ];
    driverOptions = {
      mode = "active-backup";
      primary = "enp1s0";
    };
  };
  networking.interfaces.bond0.useDHCP = true;

  # Prevent dhcpcd from falling back to a 169.254.x.x link-local address when DHCP is slow.
  # Without this, dhcpcd settles on link-local if the bond isn't ready yet and won't retry DHCP.
  networking.dhcpcd.extraConfig = "noipv4ll";

  # Wait for bond0 carrier before starting dhcpcd (bond takes a moment to come up at boot)
  systemd.services.dhcpcd = {
    after = [ "sys-subsystem-net-devices-bond0.device" ];
    wants = [ "sys-subsystem-net-devices-bond0.device" ];
  };

  # Power management
  powerManagement = {
    powertop.enable = true;            # Auto-tune power settings at boot
    cpuFreqGovernor = "powersave";     # Favor low frequencies, still allows turbo when needed
  };
  services.thermald.enable = true;     # Intel thermal daemon — keeps temps in check under load
  boot.blacklistedKernelModules = [
    "iwlwifi"    # WiFi (always on Ethernet)
    "btusb"      # Bluetooth USB
    "bluetooth"  # Bluetooth stack
    "i8042"      # PS/2 keyboard controller
    "serio"      # PS/2 serial I/O
  ];
}
