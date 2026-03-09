{ lib, ... }:
{
  # Raspberry Pi Zero 2W hardware configuration
  # - Inky Impression 7.3" display (SPI)
  # - MAX98357A I2S DAC for audio output
  # - Optional I2C sensors

  # SD card filesystem layout with wear reduction
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" "commit=60" ];
    };
    "/boot/firmware" = {
      device = "/dev/disk/by-label/FIRMWARE";
      fsType = "vfat";
      options = [ "nofail" "noauto" ];
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "nosuid" "nodev" "size=64M" ];
    };
  };

  hardware.raspberry-pi.config = {
    all = {
      # Memory and power optimization
      options = {
        gpu_mem.value = 16;
        gpu_freq_min.value = 50;
        initial_turbo.value = 30;
      };

      # Disable onboard audio, LED, enable I2C
      base-dt-params = {
        audio.enable = false;
        act_led_trigger.value = "none";
        i2c_arm = {
          enable = true;
          value = "on";
        };
      };

      # Enable SPI, I2S DAC, disable Bluetooth
      dt-overlays = {
        spi = {
          enable = true;
          params = { };
        };
        hifiberry-dac = {
          enable = true;
          params = { };
        };
        disable-bt = {
          enable = true;
          params = { };
        };
      };
    };
  };

  # Enable ALSA only (no PulseAudio/PipeWire needed)
  hardware.alsa.enable = lib.mkDefault true;
  hardware.pulseaudio.enable = false;
  services.pipewire.enable = false;
}
