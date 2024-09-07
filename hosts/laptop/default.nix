{ config, pkgs, lib, ... }:
{
  imports = [
    ./hardware                              # CPU, graphics, peripherals, etc
    ./filesystem                            # Partitioning, etc
    ../../config/nixos.nix                  # Default nixos settings

    # Users
    ./bphenriques
  ];

  networking.hostName = "bphenriques-laptop";

  # Boot
  boot = {
    supportedFilesystems.zfs = true;
    kernelPackages = pkgs.linuxPackages_6_6;
    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      configurationLimit = 5;

      # Windows To Go on a external drive. I usually turn-off physically when not in use.
      # 1. `sudo fdisk -l` to get the device where EFI System is.
      # 2. `sudo blkid {device}` to get the UUID field.
      extraEntries = ''
        menuentry "Windows 11" {
          search --fs-uuid --no-floppy --set=root 171F-2B1D
          chainloader (''${root})/EFI/Microsoft/Boot/bootmgfw.efi
        }
      '';
    };

    loader.grub2-theme = {
      enable = true;
      theme = "stylish";
      footer = true;
    };
  };
  custom.system.graphical-boot = {
    enable = true;
    theme = "sphere";
  };

  # Desktop environment
  services = {
    xserver.enable = true;
    desktopManager.plasma6.enable = true;
    displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
      defaultSession = "plasma";
    };
  };
  environment.plasma6.excludePackages = with pkgs.kdePackages; [ elisa plasma-browser-integration ];

  # Update firmware. Use fwupdmgr
  services.fwupd.enable = true;

  # Gaming
  custom.profiles.gaming.enable = true;

  # Development
  virtualisation.docker.enable = true;
  hardware.nvidia-container-toolkit.enable = true;

  # System-wide secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users and groups
  users.mutableUsers = false;

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}

