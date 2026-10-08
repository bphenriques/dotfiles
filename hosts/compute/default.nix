{ config, pkgs, inputs, private, ... }:
{
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./hardware
    ./disko.nix
    ./users.nix
    ./ollama-models.nix
    ../../profiles/nixos/headless.nix
    ../../profiles/nixos/ups-client.nix
    ./selfhost
    ./microvm/host.nix
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

  # Networking
  services.openssh.openFirewall = false; # Firewall is managed manually to ensure SSH is restricted (e.g., podman cant access)
  networking.firewall.interfaces = {
    bond0.allowedTCPPorts = [ 22 ];
    wg0.allowedTCPPorts = [ 22 ];   # the emergency path if the LAN side is wedged
  };

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
