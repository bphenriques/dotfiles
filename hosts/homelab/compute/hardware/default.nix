{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix  # Output of nixos-generate-config --root /mnt
  ];

  hardware.enableRedistributableFirmware = true;  # Misc drivers
  services.fwupd.enable = true;                   # Updates firmwares: `fwupdmgr`.

  # Disk
  services.fstrim.enable = true;       # Weekly TRIM for NVMe longevity
  services.smartd.enable = true; # TODO check prometheus alerting once I introduce it

  # RAM
  zramSwap = {
    enable = true;
    memoryPercent = 25;                # ~8GB on 32GB RAM - OOM safety net without NVMe wear
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
    interfaces = [ "enp1s0" "enp1s1" ];
    driverOptions.mode = "balance-alb";  # switch to 802.3ad after configuring on the switch (cleaner)
  };
  networking.interfaces.bond0.useDHCP = true;

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
