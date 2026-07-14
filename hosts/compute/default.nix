{ config, pkgs, lib, self, private, ... }:
{
  imports = [
    ./hardware
    ./disko.nix
    ./users
    ../../profiles/nixos
    ../../profiles/nixos/headless
    ./selfhost
    ./microvm.nix
  ];

  # Basic setup
  networking.hostName = "compute";
  boot = {
    kernelPackages = pkgs.linuxPackages_7_1;
    loader.systemd-boot = {
      enable = true;
      editor = false;
      configurationLimit = 10;
    };
  };

  # Secrets
  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.keyFile = "/var/lib/sops-nix/system-keys.txt";
  };

  # Users
  users.mutableUsers = false;
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
