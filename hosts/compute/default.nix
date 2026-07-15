{ config, pkgs, lib, self, inputs, private, ... }:
{
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./hardware
    ./disko.nix
    ./users
    ../../profiles/nixos/base.nix
    ../../profiles/nixos/headless.nix
    ./selfhost
    ./microvm.nix
  ];

  # Basic setup
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
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
