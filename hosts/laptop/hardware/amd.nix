_:
{
  # Known issue: TSC clocksource is unstable on this laptop (Legion Slim 5 14APH8).
  # The kernel falls back to HPET due to massive TSC warp between CPU cores (~2.8s skew).
  # This is a Lenovo BIOS/AGESA bug affecting many AMD Ryzen laptops.
  # See: https://forums.lenovo.com/t5/Other-Linux-Discussions/Unusable-TSC-on-P14s-and-X13-with-the-latest-LTS-kernel/m-p/5064905
  # Do NOT force `tsc=reliable clocksource=tsc` - causes crashes and negative time deltas.
  boot.kernelParams = [
    "amdgpu.sg_display=0" # Fixes flickering or stays white (https://wiki.archlinux.org/title/AMDGPU)
    "amdgpu.dpm=1"        # Dynamic Power Play: Dynamically adjust GPU based on the current demand. Preview with `powerprofilesctl list-actions` and then check `amdgpu_dpm`
  ];

  # Integrated GPU: AMD
  hardware.amdgpu = {
    initrd.enable = true;
    opencl.enable = true;
  };

  # CPU: AMD
  services.power-profiles-daemon.enable = true;
  services.auto-epp = {
    enable = true;
    settings.Settings = { # See `cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_available_preferences`
      epp_state_for_AC = "balance_performance";
      epp_state_for_BAT = "power";
    };
  };
}
