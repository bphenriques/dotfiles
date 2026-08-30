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
    ../../profiles/nixos/base.nix
    ../../profiles/nixos/headless.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_6_18;
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

  environment.systemPackages = [ inputs.disko.packages.${pkgs.stdenv.hostPlatform.system}.disko ];

  system.stateVersion = "26.05";
}
