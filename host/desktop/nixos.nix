{ config, pkgs, ... }:

let
  hostDir = "/home/${config.user.name}/.dotfiles/host/desktop";
in
{
  imports = [ ./hardware-configuration.nix ];

  # Bootloader
  boot.loader.grub = {
    enable = true;
    device = "/dev/sda";
    useOSProber = true;
  };

  # Latest kernel (aka the one pinned under flake.lock)
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Hardware
  ## Disk management
  services.fstrim.enable = true;              # Trim SSD because for some reason is not a default :shrug:
  boot.supportedFilesystems = [ "ntfs" ];     # Support regular Windows FS

  ## Video Driver
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.forceFullCompositionPipeline = true; # Fixes screen flickering

  ## Mouse - Using solaar and input-remapper to control my mouse's side buttons.
  modules.services = {
    solaar.enable = true;
    input-remapper.enable = true;
  };

  # Terrible Hack as workaround to readonly FS: https://github.com/sezanzeb/input-remapper/issues/663
  # mkOutOfStoreSymlink allows me to create a file outside of the store. I.e., to the actual file in the repo.
  home-manager.users.${config.user.name} = { config, ... }: { # ensure config is within home-manager's context
    xdg.configFile = {
      "input-remapper/config.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/config.json";
      "input-remapper/presets/Logitech G305/Media.json".source = config.lib.file.mkOutOfStoreSymlink "${hostDir}/input-remapper/Media.json";
    };
  };

  # Basic settings
  user.name = "bphenriques";
  networking.hostName = "bphenriques-desktop";

  # The release version of the first install of this system. Leave as it is!
  system.stateVersion = "22.11";
}
