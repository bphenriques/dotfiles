{ config, pkgs, lib, self, ... }:
let
  wallpapers = self.private.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "whale-sunset" "watch-tower" ];
  };
in
{
  imports = [
    ./hardware          # CPU, graphics, peripherals, etc
    ./filesystem        # Partitioning, etc
    ../../config/nixos  # Default nixos settings

    # Users
    ./bphenriques
  ];

  networking.hostName = "bphenriques-laptop";

  # Boot: See what it is taking most time: `systemd-analyze critical-chain`
  boot = {
    supportedFilesystems.zfs = true;
    kernelPackages = pkgs.linuxPackages_6_10;
    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      configurationLimit = 5;

      # I have Windows To Go on a external drive. I turn it off when not in use to reduce wear-and-tear.
      # 1. `sudo fdisk -l` to get the device where "EFI System" is.
      # 2. `sudo blkid {device}` to get the UUID field.
      extraEntries = ''
        menuentry "Windows 11" {
          search --fs-uuid --no-floppy --set=root 38CB-E581
          chainloader (''${root})/EFI/Microsoft/Boot/bootmgfw.efi
        }
        menuentry "BIOS Setup" --class efi {
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
  services.xserver.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.displayManager.defaultSession = "plasma";
  environment.plasma6.excludePackages = with pkgs.kdePackages; [ elisa plasma-browser-integration ];

  # Login Screen
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.theme = "sddm-astronaut-theme";
  environment.systemPackages = [
   # https://github.com/Keyitdev/sddm-astronaut-theme/blob/master/theme.conf
   # It is possible to override the package and set themeConfig. For now, I will iterate like this.
   pkgs.sddm-astronaut
   (pkgs.writeTextDir "share/sddm/themes/sddm-astronaut-theme/theme.conf.user" ''
     [General]
     background=${wallpapers}/share/wallpapers/watch-tower.png
     FullBlur="false"
     PartialBlur="false"
     FormPosition="center"
   '')
 ];

  # Gaming
  custom.profiles.gaming.enable = true;
  custom.proton-run.enable = true;
  custom.proton-run.defaultProtonDir = "/mnt/games/GlobalProton";

  # Development
  virtualisation.docker.enable = true;
  virtualisation.docker.enableOnBoot = false;        # Dont need it all the time: systemctl start docker
  hardware.nvidia-container-toolkit.enable = true;   # Enable nvidia runtime integration.

  # System-wide secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users and groups
  users.mutableUsers = false;

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}

