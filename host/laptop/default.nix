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
    ./hardware-configuration-extra.nix    # Extra configurations considering the hardware of the laptop
    ./disk-config.nix                     # Disk layout. Disko sets the boot.loader.grub.devices automatically.
    ../../nixos/config                    # My default nixos settings
    ./home.nix
  ];

  networking.hostName = "bphenriques-laptop";

  # Bootloader
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
    gfxmodeEfi = "2880x1800,auto";
    gfxmodeBios = "2880x1800,auto";
    configurationLimit = 5;
  };
  #boot.loader.grub2-theme = {
  #  enable = true;
  #  theme = "vimix";
  #  footer = true;
  #};

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

  # User
  # TODO: check https://github.com/iynaix/dotfiles/blob/main/nixos/users.nix
  users.users.${username} = {
    isNormalUser = true;
    initialPassword = "password";
    description = username;
    extraGroups = [ "wheel" "networkmanager" ];
  };
  home-manager.users.${username} = { imports = [ ./home.nix ]; };

  # The release version of the first install of this system. Leave as it is!
  system.stateVersion = "24.05";
}
