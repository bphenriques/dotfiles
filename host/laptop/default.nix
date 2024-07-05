{ config, pkgs, ... }:

# Laptop Legion Slim 5 Gen 8 (14" AMD) (14APH8-224):
# - AMD Ryzen™ 7 7840HS
# - 32GB RAM
# - AMD Radeon™ 780M
# - NVIDIA® GeForce RTX™ 4060 8GB


# # https://gitlab.com/usmcamp0811/dotfiles/-/tree/nixos/modules/nixos/tools?ref_type=heads
{
  imports = [
    ./hardware-configuration.nix          # Output of nixos-generate-config --root /mnt
    ./hardware-configuration-extra.nix    # Extra configurations considering the hardware of the laptop
    ./disko.nix                           # Disk layout. Disko sets the boot.loader.grub.devices automatically.
    ../../nixos                           # My default nixos settings
    ./bphenriques.nix                     # Home
    ./impermanence.nix
    ./zfs.nix                             # Service
  ];

  networking.hostName = "bphenriques-laptop";

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

  # Update firmware. Use fwupdmgr
  services.fwupd.enable = true;

  # The release version of the first install of this system. Leave as it is!
  system.stateVersion = "24.05";
}

# File browser: https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/nemo.nix#L79
