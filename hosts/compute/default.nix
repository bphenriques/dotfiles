{ config, pkgs, inputs, private, ... }:
{
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./hardware
    ./disko.nix
    ./users.nix
    ./ollama-models.nix
    ../../profiles/nixos/headless.nix
    ./selfhost
    ./microvm/host.nix
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

  # Networking
  # openFirewall's rule is unqualified, which also lets podman containers reach the host's SSH.
  services.openssh.openFirewall = false;
  networking.firewall.interfaces = {
    bond0.allowedTCPPorts = [ 22 ];
    wg0.allowedTCPPorts = [ 22 ];   # the emergency path if the LAN side is wedged
  };

  # Users
  nix.settings.trusted-users = [ config.users.users.bphenriques.name ];

  system.stateVersion = "25.11"; # The release version of the first install of this system!
}
