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
    ./disko.nix
    ./users.nix
    ./firewall.nix
    ./services
    ../../profiles/nixos/base.nix
    ../../profiles/nixos/headless.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_7_1;
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

  system.stateVersion = "26.05";
}
