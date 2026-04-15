_:
{
  # Known issue: TSC clocksource is unstable on this laptop (Legion Slim 5 14APH8).
  # The kernel falls back to HPET due to massive TSC warp between CPU cores (~2.8s skew).
  # This is a Lenovo BIOS/AGESA bug affecting many AMD Ryzen laptops. See: https://forums.lenovo.com/t5/Other-Linux-Discussions/Unusable-TSC-on-P14s-and-X13-with-the-latest-LTS-kernel/m-p/5064905
  # Do NOT force `tsc=reliable clocksource=tsc` - causes crashes and negative time deltas.
  boot.kernelParams = [
    "amdgpu.sg_display=0" # Fixes flickering or stays white (https://wiki.archlinux.org/title/AMDGPU)
  ];

  # Integrated GPU: AMD
  hardware.amdgpu = {
    initrd.enable = true;
    opencl.enable = true;
  };

  # CPU: AMD
  # auto-epp: automatically switches EPP based on AC/battery (balance_performance on AC, power on battery).
  # power-profiles-daemon: provides `powerprofilesctl` API but auto-epp overrides its EPP settings.
  # Both coexist: auto-epp handles the day-to-day AC/battery switching; PPD is kept for compatibility.
  services.power-profiles-daemon.enable = true;
  services.auto-epp = {
    enable = true;
    settings.Settings = { # See `cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_available_preferences`
      epp_state_for_AC = "balance_performance";
      epp_state_for_BAT = "power";
    };
  };
}
