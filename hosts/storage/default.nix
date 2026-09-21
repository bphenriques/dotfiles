{
  config,
  inputs,
  pkgs,
  private,
  ...
}:
{
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./hardware
    ./disko
    ./firewall.nix
    ./shares.nix
    ./users.nix
    ./selfhost
    ./services
    ../../profiles/nixos/headless.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_6_18; # Strictly LTS to avoid rebooting and spinning down disks.
    loader.systemd-boot = {
      enable = true;
      editor = false;
      configurationLimit = 10;
    };
  };

  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.keyFile = "/var/lib/sops-nix/system-keys.txt";
  };

  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  environment.systemPackages = [
    pkgs.gptfdisk # sgdisk: the degraded-pool runbook partitions a replacement by hand
  ];

  system.stateVersion = "26.05";
}
