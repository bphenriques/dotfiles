{ config, pkgs, inputs, private, ... }:
{
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./hardware
    ./disko.nix
    ./users.nix
    ./firewall.nix
    ../../profiles/nixos/headless.nix
    ../../profiles/nixos/ups-client.nix
    ./selfhost
  ];

  # Basic setup
  boot = {
    kernelPackages = pkgs.linuxPackages_7_2;
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

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
