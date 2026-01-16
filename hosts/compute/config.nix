{ config, pkgs, ... }:
{
  imports = [
    ./hardware
    ./disko.nix
    ../../config/nixos
    ../../config/nixos/headless
    ./services

    # Users
    ./bphenriques
  ];

  # TODO: https://blog.aldnav.com/blog/going-headless-with-nixos/

  # Auto-reboot in case something wrong happens and ensure watchdog is enabled.
  boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];
  systemd.settings.Manager.RuntimeWatchdogSec = "30s";

  # Core
  networking.hostName = "compute";
  boot = {
    kernelPackages = pkgs.linuxPackages_6_18;
    loader.systemd-boot = {
      enable = true;
      editor = false;
      configurationLimit = 10;
    };
  };

  # Homelab integration
  custom.fileSystems.homelab.enable = true;

  # Sops secrets for homelab authentication
  sops.secrets.homelab_samba_username = { };
  sops.secrets.homelab_samba_password = { };  

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
