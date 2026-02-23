{ config, pkgs, lib, self, ... }:
let
  networking = import ../networking.nix;
in
{
  imports = [
    ./hardware.nix
    ./microvm.nix
    ../../../config/common.nix
    ../../../config/nixos/headless/default.nix
    ./pocket-id.nix
  ];

  # Network configuration
  networking = {
    hostName = "auth";
    nameservers = networking.cloudflare.nameservers;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 80 ];
      allowedUDPPorts = [ ];
    };
  };

  # Secrets - age key auto-generated on first boot, stored in persistent /var
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age = {
    keyFile = "/var/lib/sops-nix/key.txt";
    generateKey = true;   # Auto-generate if missing
    sshKeyPaths = [];     # Don't use SSH host keys (ephemeral in microvm)
  };

  # Users - root access via SSH key for management
  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3"
  ];

  system.stateVersion = "25.11";
}
