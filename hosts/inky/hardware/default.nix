{ lib, ... }:
{
  # Raspberry Pi Zero 2W hardware configuration
  # - Inky Impression 7.3" display (SPI)
  # - MAX98357A I2S DAC for audio output
  # - Optional I2C sensors

  # SD card filesystem layout (standard Raspberry Pi setup)
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
    "/boot/firmware" = {
      device = "/dev/disk/by-label/FIRMWARE";
      fsType = "vfat";
      options = [ "nofail" "noauto" ];
    };
  };

  hardware.raspberry-pi.config = {
    all = {
      # Memory optimization - reduce GPU memory (no graphical use)
      options = {
        gpu_mem = {
          value = 16;
        };
      };

      # Enable SPI for Inky Impression display
      dt-overlays = {
        spi = {
          enable = true;
          params = { };
        };
      };

      # Enable I2C for sensors
      base-dt-params = {
        i2c_arm = {
          enable = true;
          value = "on";
        };
      };
    };
  };

  # I2S audio output for MAX98357A DAC
  # The hifiberry-dac overlay is compatible with generic I2S DACs like MAX98357A
  boot.kernelParams = [ "snd_bcm2835.enable_headphones=0" ];

  # Load I2S audio modules
  boot.kernelModules = [ "snd-soc-bcm2835-i2s" ];

  # Enable ALSA
  hardware.alsa.enable = lib.mkDefault true;
}
