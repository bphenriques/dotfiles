{ pkgs, config, ... }:

{
  imports = [
    ./hardware-configuration.nix    # Output of nixos-generate-config --root /mnt
    ./graphics.nix                  # AMD iGPU + Nvidia dGPU
    ./peripherals.nix               # Mouse / Keyboard / etc
  ];

  # Networking
  networking.networkmanager.wifi = {
    powersave = true;
    macAddress = "preserve";  # The default. I am fine as this laptop stays most of the times at home.
  };

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Power
  # AMD has better battery life with PPD over TLP: https://community.frame.work/t/responded-amd-7040-sleep-states/38101/13
  services.power-profiles-daemon.enable = true;
  services.auto-epp = {
    enable = true;
    settings.Settings = { # See `cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_available_preferences`
      epp_state_for_AC = "balance_performance";
      epp_state_for_BAT = "power";
    };
  };
  # TODO: Notifications when it is too low
  services.upower = {
    enable = true;
    percentageLow = 30;
    percentageCritical = 20;
    percentageAction = 10;
    criticalPowerAction = "PowerOff";
  };
}
