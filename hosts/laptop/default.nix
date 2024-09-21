{ config, pkgs, lib, self, ... }:
{
  imports = [
    ./hardware                              # CPU, graphics, peripherals, etc
    ./filesystem                            # Partitioning, etc
    ../../config/nixos.nix                  # Default nixos settings

    # Users
    ./bphenriques
  ];

  networking.hostName = "bphenriques-laptop";


  #TODO SDDM: https://github.com/stepanzubkov/where-is-my-sddm-theme
  # https://github.com/surajmandalcell/elegant-sddm
  # https://github.com/HeitorAugustoLN/catppuccin-where-is-my-sddm-theme

  environment.systemPackages = [
    self.pkgs.sddm-eucalyptus-drop
    (pkgs.writeTextDir "share/sddm/themes/sddm-eucalyptus-drop/theme.conf.user" ''
      [General]
      background=${self.pkgs.dotfiles-wallpapers}/share/wallpapers/whale-sunset.jpg
      FullBlur="true"
      PartialBlur="false"
      FormPosition="center"
    '')
  ];
  # https://gitlab.com/Matt.Jolly/sddm-eucalyptus-drop/-/blob/master/theme.conf?ref_type=heads

  # Boot
  boot = {
    supportedFilesystems.zfs = true;
    kernelPackages = pkgs.linuxPackages_6_6;
    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      configurationLimit = 5;

      # I have Windows To Go on a external drive. I turn it off when not in use to reduce wear-and-tear.
      # 1. `sudo fdisk -l` to get the device where EFI System is.
      # 2. `sudo blkid {device}` to get the UUID field.
      extraEntries = ''
        menuentry "Windows 11" {
          search --fs-uuid --no-floppy --set=root 171F-2B1D
          chainloader (''${root})/EFI/Microsoft/Boot/bootmgfw.efi
        }

        menuentry "Firmware settings" --class efi {
          fwsetup
        }
        menuentry "Reboot" --class restart {
          reboot
        }
        menuentry "Shutdown" --class shutdown {
          halt
        }
      '';
    };
  };
  custom.system.boot-theme = {
    enable = true;
    theme = "angular";
  };

  # Desktop environment
  services = {
    xserver.enable = true;
    desktopManager.plasma6.enable = true;
    displayManager = {
      sddm.enable = true;
      sddm.theme = "sddm-eucalyptus-drop";
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

