{ config, pkgs, ... }:
# # https://gitlab.com/usmcamp0811/dotfiles/-/tree/nixos/modules/nixos/tools?ref_type=heads
# TODO: Laptop related stuff? https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/laptop/default.nix
# Power profile? https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/modules/nixos/hardware/upower/default.nix
# File browser: https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/nemo.nix#L79
# TODO: check boot: https://github.com/adi1090x/plymouth-themes?tab=readme-ov-file. Like 70, 71, 62 63 5
{
  imports = [
    ./hardware/hardware-configuration.nix   # Output of nixos-generate-config --root /mnt
    ./hardware/graphics.nix                 # AMD + Nvidia
    ./hardware/misc.nix                     # Other hardware settings without a especific category
    ./hardware/peripherals.nix              # Mouse / Keyboard etc

    ./disko.nix                             # Instructions on how to format the disk
    ./filesystem.nix                        # More settings regarding the disk.
    ../../config/nixos.nix                  # Default nixos settings
    ./bphenriques                           # User
  ];

  networking.hostName = "bphenriques-laptop";

  # Boot
  boot = {
    supportedFilesystems.zfs = true;
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      gfxmodeEfi = "1440x900,auto";
      gfxmodeBios = "1440x900,auto";
      configurationLimit = 5;
    };
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
  environment.persistence."${config.custom.impermanence.dataLocation}".directories = [ "/var/lib/sops-nix" ];
  sops.age.keyFile = "${config.custom.impermanence.dataLocation}/var/lib/sops-nix/system-keys.txt";

  # Users
  sops.secrets.user_bphenriques_password.neededForUsers = true;
  users.mutableUsers = false;
  users.users.bphenriques = {
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets.user_bphenriques_password.path;
    uid = 1000;
    description = "bphenriques";
  };

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}

