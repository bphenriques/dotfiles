{ config, pkgs, ... }:
# TODO: Laptop related stuff? https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/laptop/default.nix
# Power profile? https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/modules/nixos/hardware/upower/default.nix
# TODO: check boot: https://github.com/adi1090x/plymouth-themes?tab=readme-ov-file. Like 70, 71, 62 63 5
# TODO: Nice boot themes: https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/initrd/default.nix#L63C12-L63C20. Is this related? https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/kernel/default.nix
{
  imports = [
    ./hardware                              # CPU, graphics, peripherals, etc
    ./filesystem                            # Partitioning, impermanence, etc
    ../../config/nixos.nix                  # Default nixos settings

    # Users
    ./bphenriques
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
  # services.dbus.packages = [ pkgs.gcr ];  # pinentry-gnome3

  # System-wide secrets
  sops.defaultSopsFile = ./secrets.yaml;
  environment.persistence."${config.custom.impermanence.dataLocation}".directories = [ "/var/lib/sops-nix" ];
  sops.age.keyFile = "${config.custom.impermanence.dataLocation}/var/lib/sops-nix/system-keys.txt";

  # Users and groups
  users.mutableUsers = false;

  system.stateVersion = "24.05"; # The release version of the first install of this system. Leave as it is!
}

