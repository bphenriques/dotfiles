{ config, pkgs, ... }:

# Laptop Legion Slim 5 Gen 8 (14" AMD) (14APH8-224):
# - AMD Ryzen™ 7 7840HS
# - 32GB RAM
# - AMD Radeon™ 780M
# - NVIDIA® GeForce RTX™ 4060 8GB

let
  username = "bphenriques";
in
{
  imports = [
    ./hardware-configuration.nix          # Output of nixos-generate-config --root /mnt
    ./hardware-configuration-extra.nix    # Extra configurations considering the specs of the laptop
    ./disk-config.nix                     # Disk layout
    ../../nixos/config                    # My default nixos settings
    ./home.nix
  ];

  # Bootloader - Devices are set by disko automatically
  # TODO: Increase font-size or increase dpi
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
    configurationLimit = 5;
  };


  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = true;
    touchpad.tapping = true;
  };

  networking.hostName = "bphenriques-laptop";
  users.users.${username} = {
    isNormalUser = true;
    initialPassword = "pass";
    description = username;
    extraGroups = [ "wheel" ];
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

  home-manager.users.${username} = {
    imports = [ ./home.nix ];
  };

  # The release version of the first install of this system. Leave as it is!
  system.stateVersion = "24.05";
}
