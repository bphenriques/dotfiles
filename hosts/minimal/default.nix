{ config, pkgs, lib, ... }:
{
  imports = [ ./hardware-configuration.nix ./disko.nix ];

  boot = {
    supportedFilesystems.zfs = true;
    initrd.postDeviceCommands = lib.mkAfter ''zfs rollback -r zroot/system/root@blank;'';
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
  };
  networking.hostName = "bphenriques-laptop";
  services.fstrim.enable = true;  # Trim SSD because it is not set by default :shrug:

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

  # ZFS
  networking.hostId = "5b318853";
  services.zfs = {
    autoScrub.enable = true;
    trim.enable = true;
  };
  fileSystems = {
    # Disko sets boot.loader.grub.devices automatically.
    "/".neededForBoot = true;
    "/nix".neededForBoot = true;
    "/boot".neededForBoot = true;
    "/persist/data/system".neededForBoot = true;
    "/persist/cache/system".neededForBoot = true;
    "/persist/data/bphenriques".neededForBoot = true;
    "/persist/cache/bphenriques".neededForBoot = true;
  };

  # Impermanence
  programs.fuse.userAllowOther = true; # Allows users to specify the "allowOther" option.
  environment.persistence = {
    "/persist/data/system" = {
      hideMounts = true;
      directories = [
        "/var/log"
        "/var/lib/nixos" # https://github.com/nix-community/impermanence/issues/178
        "/var/lib/NetworkManager"
      ];

      files = [ "/etc/machine-id" ];
    };

    "/persist/cache/system" = {
      hideMounts = true;
      directories = [
        "/var/lib/systemd/coredump"
        "/var/lib/upower"
      ];
      files = [ ];
    };
  };

  # Users and groups
  users.mutableUsers = false;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    password = "password";
  };

  # Align permissions
  systemd.tmpfiles.rules = [
    "z /persist/data/bphenriques   0700 bphenriques users"
    "z /persist/cache/bphenriques  0700 bphenriques users"
  ];

  home-manager.users.bphenriques = {
    programs.gpg.enable = true;
    services.gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };

    systemd.user.tmpfiles.rules = [
      "z /home/bphenriques/.ssh          0700 bphenriques users"
      "z /home/bphenriques/.gnupg        0700 bphenriques users"
    ];

    home.persistence."/persist/data/bphenriques" = {
      allowOther = true;
      directories = [
        ".local/share/nix" # trusted settings and repl history
        ".config/systemd"  # systemd timers
        ".ssh"
        ".gnupg"

        # Move up the list? That is the last test
        ".dotfiles"
        "Desktop"
        "Downloads"
      ];
    };

    home.persistence."/persist/cache/bphenriques" = {
      allowOther = true;
      directories = [
        ".cache/nix"
      ];
      files = [
        ".bash_history"
      ];
    };

    home.stateVersion = "24.05";
  };

  system.stateVersion = "24.05";
}
