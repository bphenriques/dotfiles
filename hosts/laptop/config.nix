{ config, pkgs, lib, self, ... }:
{
  imports = [
    ./hardware                        # CPU, graphics, peripherals, etc
    ./filesystem                      # Partitioning, etc
    ../../nixos                       # Default nixos settings
    ../../nixos/desktop-environment   # My desktop environment
    ../../nixos/desktop               # The usual desktop applications

    # Users
    ./bphenriques
  ];

  networking.hostName = "bphenriques-laptop";

  # Boot: See what it is taking most time: `systemd-analyze critical-chain`
  boot = {
    supportedFilesystems.zfs = true;
    kernelPackages = pkgs.linuxPackages_6_12;

    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      configurationLimit = 10;

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

  # Login Screen
  environment.systemPackages = [
   (pkgs.writeScriptBin "reboot-to-windows" ''
     #!${pkgs.stdenv.shell}
     sudo grub-reboot "Windows 11" && reboot $@
   '')
   (pkgs.writeScriptBin "reboot-to-bios" ''
      #!${pkgs.stdenv.shell}
      sudo grub-reboot "BIOS Setup" && reboot $@
    '')
 ];

  # Gaming
  custom.proton-run.enable = true;
  custom.proton-run.defaultProtonDir = "/mnt/games/GlobalProton";

  # Docker
  virtualisation.docker.enable = true;
  virtualisation.docker.enableOnBoot = false;        # Dont need it all the time: systemctl start docker
  hardware.nvidia-container-toolkit.enable = true;   # test: docker run --rm --device=nvidia.com/gpu=all ubuntu nvidia-smi

  # System-wide secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users and groups
  users.mutableUsers = false;

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}

