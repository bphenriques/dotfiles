{ config, pkgs, ... }:
# # https://gitlab.com/usmcamp0811/dotfiles/-/tree/nixos/modules/nixos/tools?ref_type=heads
  # TODO: Laptop related stuff? https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/laptop/default.nix
  # Power profile? https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/modules/nixos/hardware/upower/default.nix
# File browser: https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/nemo.nix#L79
# TODO: check boot: https://github.com/adi1090x/plymouth-themes?tab=readme-ov-file. Like 70, 71, 62 63 5

let
  homeNasIp = "192.168.68.53";
in
{
  imports = [
    ./hardware/hardware-configuration.nix          # Output of nixos-generate-config --root /mnt
    ./hardware/graphics.nix
    ./hardware/connectivity.nix
    ./hardware/peripherals.nix

    ./filesystem.nix                     # Disk layout. Disko sets the boot.loader.grub.devices automatically.
    ../../nixos                                    # Default nixos settings
    ./users.nix
    ./secrets
  ];

  networking.hostName = "bphenriques-laptop";
  networking.hosts = {
    homeNasIp = [ "home-nas" ];
  };

  # Bootloader
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
    gfxmodeEfi = "1440x900,auto";
    gfxmodeBios = "1440x900,auto";
    configurationLimit = 5;
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

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = true;
  };

  # Home
  custom.home-remote-disks = {
    enable = false;
    smbCredentialsOwnerUsername = "bphenriques";
    uid = 1000;
    guid = 100;
    locations = [
      { mountPoint = "/home/bphenriques/bphenriques"; device = "//${homeNasIp}/bphenriques"; }
      { mountPoint = "/mnt/home-media";               device = "//${homeNasIp}/media"; }
      { mountPoint = "/mnt/home-shared";              device = "//${homeNasIp}/shared"; }
    ];
  };

  # Update firmware. Use fwupdmgr
  services.fwupd.enable = true;

  # Development
  custom.profiles.development.enable = true;
  hardware.nvidia-container-toolkit.enable = true;

  # Gaming
  custom.profiles.gaming.enable = true;

  # ZFS
  networking.hostId = "5b318853";
  boot = {
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    supportedFilesystems.zfs = true;
  };
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };

  # https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/impermanence/default.nix
  # Imper bla bla : https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L59
  # https://github.com/search?q=repo%3Athexyno%2Fnixos-config%20ragon.persist&type=code
    # For when I use tmpfs: https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L34

  # https://gitlab.com/usmcamp0811/dotfiles/-/tree/nixos/modules/nixos/hardware?ref_type=heads
  # https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/impermanence/default.nix
  # https://github.com/AntonHakansson/nixos-config/blob/main/modules/core/zfs/default.nix#L60
  # Imper bla bla : https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L59
  # This one is nice: https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L35
  # https://github.com/search?q=repo%3Athexyno%2Fnixos-config%20ragon.persist&type=code
  # https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/impermanence/default.nix
  custom.impermanence = {
    enable = true;
    rootBlankSnapshot = "zroot/system/root@blank";
    configLocation = "/persist/config/system";
    cacheLocation = "/persist/cache/system";
  };
  #  services.sanoid = lib.mkIf cfg.snapshots {
  #    enable = true;
  #    datasets = {
  #      "zroot/persist" = {
  #        hourly = 50;
  #        daily = 15;
  #        weekly = 3;
  #        monthly = 1;
  #      };
  #    };
  #  };

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}

