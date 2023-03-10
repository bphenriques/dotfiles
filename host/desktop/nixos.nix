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

  # Disk management
  fileSystems."/mnt/data" = {
    device = "/dev/disk/by-label/data";      # Previously formatted.
    fsType = "auto";
    options = [ "nosuid" "nodev" "nofail" ]; # Standard security plus allow booting if fails.
  };
  services.fstrim.enable = true;             # Trim SSD because for some reason is not a default :shrug:

  # Nvidia drivers
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.forceFullCompositionPipeline = true; # Fixes screen issues

  user.name = "bphenriques";

  # Networking
  networking.hostName = "bphenriques-desktop";

  # Keymapping
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  #users.users.bphenriques = {
  #  isNormalUser = true;
  #  extraGroups = [ "networkmanager" "wheel" "qbittorrent" ];
  #};


  # TODO: Automatically create Downloads directory on the secundary disk
  # TODO: Autoamtically set torrent parth location to that same place

  # TODO explore:
  #environment.homeBinInPath
  # https://nixos.org/manual/nixos/stable/index.html#sec-x11-auto-login

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11";
}
