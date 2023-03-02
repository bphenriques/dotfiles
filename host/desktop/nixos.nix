{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # Bootloader.
  boot = {
    supportedFilesystems = [ "ntfs" ];
    loader.grub = {
      enable = true;
      device = "/dev/sda";
      useOSProber = true;
    };
  };

  # Display
  services.xserver = {
    enable = true; # Enable the X11 windowing system.
    videoDrivers = [ "nvidia" ];

    # Plasma Desktop Environment
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
  };
  hardware.opengl.enable = true;


  hardware.nvidia.forceFullCompositionPipeline = true; # Fixes screen issues

  # Networking
  networking = {
    hostName = "bphenriques-desktop";
    networkmanager.enable = true;
  };

  # Keymapping
  services.xserver = {
    layout = "us";
    xkbVariant = "";
    xkbOptions = "caps:ctrl_modifier";
  };

  # Sound
  sound.enable = true;
  hardware.pulseaudio.enable = false;

  # What does this do?
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };


  # TODO verify:
  #environment.homeBinInPath


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11";
}
