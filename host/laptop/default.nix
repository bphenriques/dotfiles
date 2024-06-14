{ config, pkgs, ... }:

# TODO: should I check https://github.com/nix-community/nixos-anywhere-examples/blob/main/configuration.nix ?
{
  imports = [
    ./hardware-configuration.nix          # As displayed when using nixos-generate-config
    ./hardware-configuration-extra.nix    # Extra configurations considering that the specs of the laptop
    ../../nixos/config
    ./home.nix
    ./desktop-environment.nix
    ./disk2.nix
  ];

  services.openssh.enable = true;

  # Basic settings
  networking.hostName = "bphenriques-laptop";
  user.name = "bphenriques";

  # Points to NAS locations
  user.musicDir = "/home/${config.user.name}/media/music/library";
  user.romsDir = "/home/${config.user.name}/media/gaming/emulation/roms";

  user.protonDefaultPrefixDir = "/mnt/data/Games/Other";

  # Bootloader - Devices are set by disko automatically
  # Review options: https://nixos.org/manual/nixos/stable/options#opt-boot.loader.grub.devices
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
    configurationLimit = 5;
  };
  #fileSystems."@/persist".neededForBoot = true;
  #fileSystems."@/var-log".neededForBoot = true;

  # Latest kernel (aka the one pinned under flake.lock)
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "amd_pstate=active"   # Enables the amd cpu scaling https://www.kernel.org/doc/html/latest/admin-guide/pm/amd-pstate.html. On recent AMD CPUs this can be more energy efficient.
    "amdgpu.sg_display=0" # Fixes flickring or stays white (https://wiki.archlinux.org/title/AMDGPU)
  ];

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = true;
    touchpad.tapping = true;
  };

  #virtualisation.docker.enableNvidia = true;

  # The release version of the first install of this system. Leave as it is!
  system.stateVersion = "24.05";
}
