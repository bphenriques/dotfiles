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
  environment.systemPackages = [ pkgs.intel_gpu_top ]; # iGPU monitoring

  networking.bonds.bond0 = {
    interfaces = [ "enp1s0" "enp1s1" ];
    driverOptions.mode = "balance-alb";  # switch to 802.3ad after configuring on the switch (cleaner)
  };

  # Power management
  powerManagement = {
    powertop.enable = true;            # Auto-tune power settings at boot
    cpuFreqGovernor = "powersave";     # Favor low frequencies, still allows turbo when needed
  };
  boot.blacklistedKernelModules = [
    "iwlwifi"    # WiFi (always on Ethernet)
    "btusb"      # Bluetooth USB
    "bluetooth"  # Bluetooth stack
  ];
}
