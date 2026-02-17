{ config, pkgs, lib, self, ... }:
{
  imports = [
    ./hardware.nix
    ./microvm.nix
    ../../config/common.nix
    ../../config/nixos/headless/default.nix
    ./services
  ];

  # Network configuration
  networking = {
    hostName = "auth";
    nameservers = [ "1.1.1.1" "8.8.8.8" ]; # Cloudflare
    firewall = {
      allowedTCPPorts = [ 443 ]; # Intentionally only expose HTTPS
      allowedUDPPorts = [ 443 ];
    };
  };

  # Secrets
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age.keyFile = "/var/lib/sops-nix/system-keys.txt";

  # Users - root access via SSH key for management
  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3"
  ];

  system.stateVersion = "25.11";
}
