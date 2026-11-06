_:
{
  # TSC is unstable here (Lenovo BIOS bug, ~2.8s core skew), so the kernel uses HPET. Never force `tsc=reliable`: it crashes.
  boot.kernelParams = [
    "amdgpu.sg_display=0" # Fixes flickering or stays white (https://wiki.archlinux.org/title/AMDGPU)
  ];

  # Integrated GPU: AMD
  hardware.amdgpu = {
    initrd.enable = true;
    opencl.enable = true;
  };

  # auto-epp owns EPP and pins amd_pstate=active; no power-profiles-daemon (it would fight over the same knob).
  services.auto-epp = {
    enable = true;
    settings.Settings = { # See `cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_available_preferences`
      epp_state_for_AC = "balance_performance";
      epp_state_for_BAT = "balance_power";
    };
  };
}
