_:
{
  boot.kernelParams = [
    "amdgpu.sg_display=0" # Fixes flickring or stays white (https://wiki.archlinux.org/title/AMDGPU)
    "amdgpu.dpm=1"        # Dynamic Power Play: Dynamically adjust GPU based on the current demand. Preview with `powerprofilesctl list-actions` and then check `amdgpu_dpm`
  ];

  # Integrated GPU: AMD
  hardware.amdgpu = {
    amdvlk.enable = true;
    amdvlk.support32Bit.enable = true;

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
